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

import Data.Typeable (Typeable)
import Data.Proxy    (Proxy(..))
import Control.Monad.Identity

class Graph spec where
  data Edges spec
  data Filters spec

data a :<>: b
  deriving (Typeable)

instance (Graph s, Graph s') => Graph (s :<>: s') where
  data Edges (s :<>: s') = ELeft (Edges s)
                         | ERight (Edges s')
  data Filters (s :<>: s') = Prod (Filters s) (Filters s')

class (Graph s, Monad m) => GraphMonad s m | s -> m where
  selectEdges :: Proxy s -> Filters s -> m [Edges s]

instance (GraphMonad s m, GraphMonad s' m) => GraphMonad (s :<>: s') m where
  selectEdges _ (Prod f f') = do
    res  <- selectEdges Proxy f
    res' <- selectEdges Proxy f'
    pure $ (ELeft <$> res) ++ (ERight <$> res')

class Fetchable a where
  type Id a :: *

data from -: label
  deriving (Typeable)
infix 9 -:

data prem :-> to
  deriving (Typeable)
infix 8 :->

class Selectable e where
  data Selector e :: *

  select :: Selector e -> e -> Bool

instance (Fetchable a, Selectable e, Fetchable b) => Graph (a -: e :-> b) where
  data Edges (a -: e :-> b) = Edge (Id a) (Id b)
  data Filters (a -: e :-> b) = F (Selector e)
