{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module GraphSpec where

import           Control.Applicative    (liftA2)
import           Control.Monad.Identity (Identity (..))
import           Data.Aeson
import           Data.List              (intersect, union)
import           Test.Hspec             (Spec, describe, it, shouldBe)
import           Test.QuickCheck        (Arbitrary (arbitrary), oneof, property)
import           Web.Ogma.Resource

import           Shared

spec :: Spec
spec = do
  checkSelectableLaws @(Edges (Character -: Relation :-> Character)) "Simple Edge"
  checkSelectableLaws @(Edges TGraph) "Composed Edges"

  checkRelation "Eq for Simple Edge" (\(x :: Edges (Character -: Relation :-> Character)) y -> x == y)
  checkRelation "Eq for Composed Edges" (\(x :: Edges TGraph) y -> x == y)

  describe "Aeson instances (Simple Edge)" $
    it "should be inverse functions of each other" $ property $
      \x -> (fromJSON . toJSON) x == Success (x ::Edges (Character -: Relation :-> Character))

  describe "Aeson instances (Composed Edges)" $
    it "should be inverse functions of each other" $ property $
      \x -> (fromJSON . toJSON) x == Success (x ::Edges TGraph)

  describe "selectEdges" $ do
    it "should select everything" $
      selectEdges everything `shouldBe`
        Identity allRes

    it "should select only Relation" $
      selectEdges (EProd everything nothing) `shouldBe`
        Identity onlyRelation

    it "should select only FriendsWith" $
      selectEdges selectFriends `shouldBe`
        Identity onlyFriends

checkSelectableLaws :: forall a. (Arbitrary a, Show (Selector a), Arbitrary (Selector a), Selectable a, Show a) => String -> Spec
checkSelectableLaws name =
  describe ("Selectable Type Laws (" ++ name ++ ")") $ do
    it "everything should validate everything" $ property $
      select (everything @a)
    it "nothing should discard everything" $ property $
      \x -> not $ select (nothing @a) x
    it "select both" $ property $
      \x y z -> select (both @a y z) x --> (select y x && select z x)

instance (Arbitrary (Id a), Arbitrary (Selector e), Arbitrary (Id b)) => Arbitrary (Selector (Edges (a -: e :-> b))) where
  arbitrary = ES <$>  arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary (Id a), Arbitrary e, Arbitrary (Id b)) => Arbitrary (Edges (a -: e :-> b)) where
  arbitrary = Edge <$>  arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary (Selector (Edges s)), Arbitrary (Selector (Edges s'))) => Arbitrary (Selector (Edges (s :<>: s'))) where
  arbitrary = EProd <$> arbitrary <*> arbitrary

instance (Arbitrary (Edges s), Arbitrary (Edges s')) => Arbitrary (Edges (s :<>: s')) where
  arbitrary = oneof [ ERight <$> arbitrary
                    , ELeft <$> arbitrary
                    ]

data Character
data Location

instance Fetchable Character where
  type Id Character = String

instance Fetchable Location where
  type Id Location = String

data Relation = FriendsWith
              | WorksFor
  deriving (Eq, Show)

data BirthPlace = BirthPlace
  deriving (Eq, Show)

instance Selectable Relation where
  data Selector Relation = Is (Maybe [Relation])
    deriving (Eq, Show)

  nothing = Is (Just [])
  everything = Is Nothing

  select (Is sel) x = maybe True (\rels -> x `elem` rels) sel

  both (Is Nothing) (Is Nothing)   = Is Nothing
  both (Is (Just x)) (Is Nothing)  = Is (Just x)
  both (Is Nothing) (Is (Just y))  = Is (Just y)
  both (Is (Just x)) (Is (Just y)) = Is . Just $ intersect x y

instance Arbitrary Relation where
  arbitrary = oneof $ pure <$> [ FriendsWith
                               , WorksFor
                               ]

instance Arbitrary (Selector Relation) where
  arbitrary = Is <$> arbitrary

instance ToJSON Relation where
  toJSON FriendsWith = String "is friend with"
  toJSON WorksFor    = String "works for"

instance FromJSON Relation where
  parseJSON (String "is friend with") = pure FriendsWith
  parseJSON (String "works for")      = pure WorksFor

instance Selectable BirthPlace where
  data Selector BirthPlace = SelectBirthPlace | NoBirthPlace
    deriving (Eq, Show)

  nothing = NoBirthPlace
  everything = SelectBirthPlace

  select SelectBirthPlace _ = True
  select _ _                = False

  both SelectBirthPlace SelectBirthPlace = SelectBirthPlace
  both _ _                               = NoBirthPlace

instance Arbitrary BirthPlace where
  arbitrary = pure BirthPlace

instance ToJSON BirthPlace where
  toJSON _ = String "is born at"

instance FromJSON BirthPlace where
  parseJSON (String "is born at") = pure BirthPlace

instance Arbitrary (Selector BirthPlace) where
  arbitrary = oneof [ pure SelectBirthPlace, pure NoBirthPlace ]

relations :: [Edges (Character -: Relation :-> Character)]
relations = [Edge "Tom" FriendsWith "Max", Edge "Max" WorksFor "Peter", Edge "Max" FriendsWith "Peter"]

birthPlaces :: [Edges (Character -: BirthPlace :-> Location)]
birthPlaces = [Edge "Tom" BirthPlace "NYC", Edge "Peter" BirthPlace "London", Edge "Max" BirthPlace "London"]

type TGraph = (Character -: Relation :-> Character)
         :<>: (Character -: BirthPlace :-> Location)

allRes :: [Edges TGraph]
allRes = [ ELeft (Edge "Tom" FriendsWith "Max")
         , ELeft (Edge "Max" WorksFor "Peter")
         , ELeft (Edge "Max" FriendsWith "Peter")
         , ERight (Edge "Tom" BirthPlace "NYC")
         , ERight (Edge "Peter" BirthPlace "London")
         , ERight (Edge "Max" BirthPlace "London")
         ]

onlyRelation :: [Edges TGraph]
onlyRelation = [ ELeft (Edge "Tom" FriendsWith "Max")
               , ELeft (Edge "Max" WorksFor "Peter")
               , ELeft (Edge "Max" FriendsWith "Peter")
               ]

onlyFriends :: [Edges TGraph]
onlyFriends = [ ELeft (Edge "Tom" FriendsWith "Max")
              , ELeft (Edge "Max" FriendsWith "Peter")
              ]

selectFriends :: Selector (Edges TGraph)
selectFriends = EProd (ES Nothing (Is (Just [FriendsWith])) Nothing) nothing

instance Named (Character -: Relation :-> Character) where
  name = "RELATION"

instance Named (Character -: BirthPlace :-> Location) where
  name = "BIRTH"

instance GraphMonad (Character -: Relation :-> Character) Identity where
  selectEdges sel = pure $ selects sel relations

instance GraphMonad (Character -: BirthPlace :-> Location) Identity where
  selectEdges sel = pure $ selects sel birthPlaces
