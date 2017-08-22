{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Web.Ogma.Resource where

import Control.Applicative    ((<|>))
import           Control.Monad.Identity
import           Data.Aeson
import           Data.Maybe             (maybe)
import           Data.Proxy             (Proxy (..))
import           Data.Typeable          (Typeable)

-- | A typeclass to filter given a dedicated 'Selector'.
class Selectable e where
  -- | The 'Selector' is a set of values which can be used to validate or
  --   discard a 'Selectable' value.
  data Selector e :: *

  -- | Given one 'Selector', says if one 'Selectable' is validated or discarded.
  select     :: Selector e -> e -> Bool
  -- | The 'Selector' which discards any input values. In other words:
  --
  --   prop> select (everything Proxy) e = True
  nothing    :: Proxy e -> Selector e
  -- | The 'Selector' which validates any input values. In other words:
  --
  --   prop> select (nothing Proxy) e = False

  everything :: Proxy e -> Selector e

-- TODO: add the following properties
-- either :: Proxy e -> Selector e -> Selector e -> Bool
-- both   :: Proxy e -> Selector e -> Selector e -> Bool
-- With the following properties:
--   prop> select (either Proxy x y) e = select x e || select y e
--   prop> select (both Proxy x y) e = select x e && select y e

selects :: (Selectable e) => Selector e -> [e] -> [e]
selects sel = filter (select sel)

class Graph spec where
  data Edges spec

instance (Selectable a, Selectable b) => Selectable (Either a b) where
  data Selector (Either a b) = Prod (Selector a) (Selector b)

  select (Prod f _) (Left x)   = select f x
  select (Prod _ f') (Right y) = select f' y

  nothing _ = Prod (nothing Proxy) (nothing Proxy)
  everything _ = Prod (everything Proxy) (everything Proxy)

data (a :: *) :<>: (b :: *)
  deriving (Typeable)

instance (Graph s, Graph s') => Graph (s :<>: s') where
  data Edges (s :<>: s') = ELeft (Edges s)
                         | ERight (Edges s')

instance (Graph s, Graph s', Selectable (Edges s), Selectable (Edges s')) => Selectable (Edges (s :<>: s')) where
  data Selector (Edges (s :<>: s')) = EProd (Selector (Edges s)) (Selector (Edges s'))

  select (EProd f _) (ELeft x)   = select f x
  select (EProd _ f') (ERight y) = select f' y

  nothing _ = EProd (nothing Proxy) (nothing Proxy)
  everything _ = EProd (everything Proxy) (everything Proxy)

class (Graph s, Monad m, Selectable (Edges s)) => GraphMonad s m where
  selectEdges :: Proxy s -> Selector (Edges s) -> m [Edges s]

instance (GraphMonad s m, GraphMonad s' m) => GraphMonad (s :<>: s') m where
  selectEdges _ (EProd f f') = do
    res  <- selectEdges Proxy f
    res' <- selectEdges Proxy f'
    pure $ (ELeft <$> res) ++ (ERight <$> res')

class Fetchable a where
  type Id a :: *

data (from :: *) -: (label :: *)
  deriving (Typeable)
infix 9 -:

data prem :-> (to :: *)
  deriving (Typeable)
infix 8 :->

instance (Fetchable a, Selectable e, Fetchable b) => Graph (a -: e :-> b) where
  data Edges (a -: e :-> b) = Edge (Id a) e (Id b)

instance (Eq (Id a), Eq (Id b), Graph (a -: e :-> b), Selectable e) => Selectable (Edges (a -: e :-> b)) where
  data Selector (Edges (a -: e :-> b)) = ES (Maybe [Id a]) (Selector e) (Maybe [Id b])

  select (ES froms sel tos) (Edge ia l ib) =
       maybe True (\ids -> ia `elem` ids) froms
    && maybe True (\ids -> ib `elem` ids) tos
    && select sel l

  nothing _ = ES (Just []) (nothing Proxy) (Just [])
  everything _ = ES Nothing (everything Proxy) Nothing


class Named a where
  name :: Proxy a -> String

instance (Named (a -: e :-> b), ToJSON (Id a), ToJSON (Id b), ToJSON e) => ToJSON (Edges (a -: e :-> b)) where
  toJSON (Edge ida label idb) = object [ "type" .= name (Proxy :: Proxy (a -: e :-> b))
                                       , "from" .= ida
                                       , "to" .= idb
                                       , "label" .= label
                                       ]

instance (Named (a -: e :-> b), FromJSON (Id a), FromJSON (Id b), FromJSON e) => FromJSON (Edges (a -: e :-> b)) where
  parseJSON (Object o) = do
    t <- o .: "type"

    if t == name (Proxy :: Proxy (a -: e :-> b))
    then Edge <$> o .: "from"
              <*> o .: "label"
              <*> o .: "to"
    else fail "not the good type"

instance (ToJSON (Edges s), ToJSON (Edges s')) => ToJSON (Edges (s :<>: s')) where
  toJSON (ELeft x)  = toJSON x
  toJSON (ERight y) = toJSON y

instance (FromJSON (Edges s), FromJSON (Edges s')) => FromJSON (Edges (s :<>: s')) where
  parseJSON json = (ELeft <$> parseJSON json) <|> (ERight <$> parseJSON json)

instance (Named (a -: e :-> b), Show (Id a), Show (Id b), Show e) => Show (Edges (a -: e :-> b)) where
  show (Edge ida label idb) = "[" ++ name (Proxy :: Proxy (a -: e :-> b)) ++ "] from " ++ show ida
    ++ " to " ++ show idb ++ " with label " ++ show label

instance (Show (Edges a), Show (Edges b)) => Show (Edges (a :<>: b)) where
  show (ERight x) = show x
  show (ELeft x)  = show x

deriving instance (Eq (Id a), Eq (Id b), Eq e) => Eq (Edges (a -: e :-> b))
deriving instance (Eq (Edges s), Eq (Edges s')) => Eq (Edges (s :<>: s'))
