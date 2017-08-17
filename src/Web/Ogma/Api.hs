{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

module Web.Ogma.Api where

import           Prelude         hiding (Foldable)
import           Servant.API
import           Data.Text       (Text)
import           Data.Aeson

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

class Foldable a where
  fold :: Int -> a -> a
  fold _ = id

instance Foldable Int
instance Foldable String
instance Foldable Text
instance Foldable Bool

class Applicative m => Expendable m a where
  expand :: Int -> a -> m a
  expand _ x = pure x

instance Applicative m => Expendable m Int
instance Applicative m => Expendable m String
instance Applicative m => Expendable m Text
instance Applicative m => Expendable m Bool

data Entity a = Complete (Identified a)
              | IdOnly (Id a)

instance WeakFunctor Entity where
  wmap f (Complete x) = Complete (wmap f x)
  wmap _ x = x

instance (Foldable a) => Foldable (Entity a) where
  fold _ (IdOnly idx) = IdOnly idx
  fold x (Complete (Identified idx value)) =
    if x < 1
    then (IdOnly idx)
    else (Complete (Identified idx (fold (x-1) value)))

data EntityList a = CompleteList [Identified a]
                  | IdOnlyList [Id a]

instance WeakFunctor EntityList where
  wmap f (CompleteList x) = CompleteList (wmap f <$> x)
  wmap _ x = x

instance (Foldable a) => Foldable (EntityList a) where
  fold _ x@(IdOnlyList _) = x
  fold x y@(CompleteList l) =
    if x < 1
    then IdOnlyList (foldI <$> l)
    else CompleteList (wmap (fold (x-1)) <$> l)
    where foldI :: Identified a -> Id a
          foldI (Identified idx _) = idx

instance (ToJSON a, ToJSON (Id a)) => ToJSON (Entity a) where
  toJSON (IdOnly idx) = toJSON idx
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
