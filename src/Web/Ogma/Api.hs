{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Web.Ogma.Api where

import           Data.Aeson
import           Data.Text     (Text)
import           Prelude       hiding (Foldable)
import           Servant.API

import           Web.Ogma.Data

type family Id a :: *

type instance Id Location = Int

data Identified a = Identified (Id a) a

instance WeakFunctor Identified where
  wmap f (Identified idx x) = Identified idx (f x)

instance (ToJSON a, ToJSON (Id a)) => ToJSON (Identified a) where
  toJSON (Identified idx value) = object [ "key" .= idx
                                         , "value" .= value
                                         ]

type PaginatedGet x y = QueryParam "offset" Int :> QueryParam "count" Int :> Get x [y]
type PaginatedFoldableGet x y = QueryParam "fold" Int :> PaginatedGet x y
type PaginatedEntitiesGet x y = QueryParam "offset" Int :> QueryParam "count" Int :> QueryParam "fold" Int :> Get x (EntityList y)
type FoldableGet x y = QueryParam "fold" Int :> Get x y

class WeakFunctor f where
  wmap :: (a -> a) -> f a -> f a

class Shrinkable a where
  shrink :: a -> a
  shrink = id

instance Shrinkable Int
instance Shrinkable String
instance Shrinkable Text
instance Shrinkable Bool

class (Shrinkable a, Applicative m) => Expendable m a where
  expand :: Int -> a -> m a
  expand _ = pure

class Fetchable m a where
  fetch :: Id a -> m a

instance Applicative m => Expendable m Int
instance Applicative m => Expendable m String
instance Applicative m => Expendable m Text
instance Applicative m => Expendable m Bool

data Entity a = Complete (Identified a)
              | IdOnly (Id a)

instance WeakFunctor Entity where
  wmap f (Complete x) = Complete (wmap f x)
  wmap _ x            = x

instance Shrinkable (Entity a) where
  shrink (Complete (Identified idx _)) = IdOnly idx
  shrink x                             = x

instance (Monad m, Expendable m a, Fetchable m a) => Expendable m (Entity a) where
  expand x y@(IdOnly idx)
    | x < 1     = pure y
    | otherwise = (Complete . Identified idx)  <$> (fetch idx >>= expand (x-1))
  expand x (Complete (Identified idx value))
    | x < 1     = pure $ IdOnly idx
    | otherwise = (Complete . Identified idx) <$> expand (x-1) value

data EntityList a = CompleteList [Identified a]
                  | IdOnlyList [Id a]

instance WeakFunctor EntityList where
  wmap f (CompleteList x) = CompleteList (wmap f <$> x)
  wmap _ x                = x

instance (Shrinkable a) => Shrinkable (EntityList a) where
  shrink (CompleteList l) = IdOnlyList (foldI <$> l)
    where foldI :: Identified a -> Id a
          foldI (Identified idx _) = idx
  shrink x = x

instance (Monad m, Expendable m a, Fetchable m a) => Expendable m (EntityList a) where
  expand x y@(IdOnlyList idxs)
    | x < 1     = pure y
    | otherwise = CompleteList <$> ((\idx -> Identified idx <$> (fetch idx >>= expand (x-1))) `mapM` idxs)
  expand x (CompleteList ids)
    | x < 1     = IdOnlyList <$> ((\(Identified idx _) ->  pure idx) `mapM` ids)
    | otherwise = CompleteList <$> ((\(Identified idx value) -> Identified idx <$> (expand $ x-1) value) `mapM` ids)

instance (ToJSON a, ToJSON (Id a)) => ToJSON (Entity a) where
  toJSON (IdOnly idx)  = toJSON idx
  toJSON (Complete ex) = toJSON ex

data Event = Event Title Description TimeInterval (Entity Location)

type instance Id Event = Text

instance ToJSON Event where
  toJSON (Event t d int s) = object [ "title" .= t
                                    , "description" .= d
                                    , "when" .= int
                                    , "where" .= s
                                    ]

type OgmaAPI =
       "events" :> QueryParam "at" Surface :> QueryParam "when" TimeInterval :> PaginatedEntitiesGet '[JSON] Event
  :<|> "events" :> QueryParam "located" Surface :> QueryParam "when" TimeInterval :> PaginatedEntitiesGet '[JSON] Event
  :<|> "event" :> Capture "eventid" (Id Event) :> FoldableGet '[JSON] Event
