{-|
Module:     Web.Ogma.Resource
Copyright:  Ogma Project 2016–2017
License:    AGPL-3
Stability:  experimental

This module provides a set of typeclasses to work with the resources exposed by
the library. The main idea is to derive an API specifications and servers.

-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

module Web.Ogma.Resource
  ( Resource(..)
  , Fetchable(..)
  , Selectable(..)
  , selects
  ) where

-- | A resource needs to be 'Fetchable' and to do that, it needs to be easily
--   identifiable. The 'Fetchable' typeclass is the entry point of the
--   'Resource' typeclass hierarchy and is here to deal with that.
class Fetchable a where
  -- | To be 'Fetchable', a 'Resource' needs to have a well identified 'Id'.
  type Id a :: *

-- | Typically, an API will give a route to get a list of a given
--   'Resource'. The 'Selectable' typeclass is here to provide a 'Selector',
--   that is a specific set of values to validate or discard a 'Resource'.
class Selectable e where
  -- | The 'Selector' is a set of values which can be used to validate or
  --   discard a 'Selectable' value.
  data Selector e :: *

  -- | Given one 'Selector', says if one 'Selectable' is validated or discarded.
  select     :: Selector e -> e -> Bool
  -- | The 'Selector' which discards any input values. In other words:
  --
  --   prop> select (everything Proxy) e = True
  nothing    :: Selector e
  -- | The 'Selector' which validates any input values. In other words:
  --
  --   prop> select (nothing Proxy) e = False

  everything :: Selector e

  -- | An aggregator for the 'Selector' type. The implementation should have the
  --   following property:
  --
  --   prop> select (both Proxy x y) e = select x e && select y e
  both   :: Selector e -> Selector e -> Selector e

selects :: (Selectable e) => Selector e -> [e] -> [e]
selects sel = filter (select sel)

-- | A 'Resource' is a data which is exposed by an API.
class (Fetchable a, Selectable a) => Resource a
