{-| Module: Web.Ogma.Graph
    License: AGPL-3
-}

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
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Web.Ogma.Graph where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.List           (intersect)
import           Data.Maybe          (maybe)
import           Data.Typeable       (Typeable)

import           Web.Ogma.Resource

class Graph spec where
  data Edges spec

data (a :: *) :<>: (b :: *)
  deriving (Typeable)

instance (Graph s, Graph s') => Graph (s :<>: s') where
  data Edges (s :<>: s') = ELeft (Edges s)
                         | ERight (Edges s')

instance (Graph s, Graph s', Selectable (Edges s), Selectable (Edges s')) => Selectable (Edges (s :<>: s')) where
  data Selector (Edges (s :<>: s')) = EProd (Selector (Edges s)) (Selector (Edges s'))

  select (EProd f _) (ELeft x)   = select f x
  select (EProd _ f') (ERight y) = select f' y

  nothing = EProd nothing nothing
  everything = EProd everything everything

  both (EProd p p') (EProd q q') = EProd (both p q) (both p' q')

deriving instance (Eq (Selector (Edges a)), Eq (Selector (Edges b))) => Eq (Selector (Edges (a :<>: b)))
deriving instance (Show (Selector (Edges a)), Show (Selector (Edges b))) => Show (Selector (Edges (a :<>: b)))

class (Graph s, Monad m, Selectable (Edges s)) => GraphMonad s m where
  selectEdges :: Selector (Edges s) -> m [Edges s]

instance (GraphMonad s m, GraphMonad s' m) => GraphMonad (s :<>: s') m where
  selectEdges (EProd f f') = do
    res  <- selectEdges f
    res' <- selectEdges f'
    pure $ (ELeft <$> res) ++ (ERight <$> res')

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

  nothing = ES (Just []) nothing (Just [])
  everything = ES Nothing everything Nothing

  both (ES x label y) (ES x' label' y') =
    let nx =  prod x x' in
    let ny = prod y y' in
      ES nx (both label label') ny
    where prod Nothing Nothing   = Nothing
          prod (Just x) Nothing  = Just x
          prod Nothing (Just y)  = Just y
          prod (Just x) (Just y) = Just $ intersect x y

deriving instance (Eq (Id a), Eq (Id b), Eq (Selector e)) => Eq (Selector (Edges (a -: e :-> b)))
deriving instance (Show (Id a), Show (Id b), Show (Selector e)) => Show (Selector (Edges (a -: e :-> b)))

class Named a where
  name :: String

instance (Named (a -: e :-> b), ToJSON (Id a), ToJSON (Id b), ToJSON e) => ToJSON (Edges (a -: e :-> b)) where
  toJSON (Edge ida label idb) = object [ "type" .= name @(a -: e :-> b)
                                       , "from" .= ida
                                       , "to" .= idb
                                       , "label" .= label
                                       ]

instance (Named (a -: e :-> b), FromJSON (Id a), FromJSON (Id b), FromJSON e) => FromJSON (Edges (a -: e :-> b)) where
  parseJSON (Object o) = do
    t <- o .: "type"

    if t == name @(a -: e :-> b)
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
  show (Edge ida label idb) = "[" ++ name @(a -: e :-> b) ++ "] from " ++ show ida
    ++ " to " ++ show idb ++ " with label " ++ show label

instance (Show (Edges a), Show (Edges b)) => Show (Edges (a :<>: b)) where
  show (ERight x) = show x
  show (ELeft x)  = show x

deriving instance (Eq (Id a), Eq (Id b), Eq e) => Eq (Edges (a -: e :-> b))
deriving instance (Eq (Edges s), Eq (Edges s')) => Eq (Edges (s :<>: s'))
