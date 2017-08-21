{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Ogma.Resource where

import Data.Typeable            (Typeable)
import Data.Proxy               (Proxy(..))
import Control.Monad.Identity
import Data.Maybe               (maybe)

class Selectable e where
  data Selector e :: *

  select     :: Selector e -> e -> Bool
  nothing    :: Proxy e -> Selector e
  everything :: Proxy e -> Selector e

class Graph spec where
  data Edges spec

instance (Selectable a, Selectable b) => Selectable (Either a b) where
  data Selector (Either a b) = Prod (Selector a) (Selector b)

  select (Prod f _) (Left x) = select f x
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

  select (EProd f _) (ELeft x) = select f x
  select (EProd _ f') (ERight y) = select f' y

  nothing _ = EProd (nothing Proxy) (nothing Proxy)
  everything _ = EProd (everything Proxy) (everything Proxy)

class (Graph s, Monad m, Selectable (Edges s)) => GraphMonad s m | s -> m where
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
