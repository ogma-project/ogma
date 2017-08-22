{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module GraphSpec where

import           Control.Monad.Identity (Identity (..))
import           Data.Aeson
import           Data.Proxy
import           Test.Hspec             (Spec, describe, it, shouldBe)
import           Test.QuickCheck        (Arbitrary (arbitrary), oneof, property)
import           Web.Ogma.Resource

import           Shared

spec :: Spec
spec = do
  checkSelectableLaws (Proxy :: Proxy (Edges (Character -: Relation :-> Character))) "Simple Edge"
  checkSelectableLaws (Proxy :: Proxy (Edges TGraph)) "Composed Edges"

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
      selectEdges proxy (everything Proxy) `shouldBe`
        Identity allRes

    it "should select only Relation" $
      selectEdges proxy (EProd (everything Proxy) (nothing Proxy)) `shouldBe`
        Identity onlyRelation

    it "should select only FriendsWith" $
      selectEdges proxy selectFriends `shouldBe`
        Identity onlyFriends

checkSelectableLaws :: (Arbitrary a, Selectable a, Show a) => Proxy a -> String -> Spec
checkSelectableLaws proxy name =
  describe ("Selectable Type Laws (" ++ name ++ ")") $ do
    it "everything should validate everything" $ property $
      select (everything proxy)
    it "nothing should discard everything" $ property $
      \x -> not $ select (nothing proxy) x

instance (Arbitrary (Id a), Arbitrary e, Arbitrary (Id b)) => Arbitrary (Edges (a -: e :-> b)) where
  arbitrary = Edge <$>  arbitrary <*> arbitrary <*> arbitrary

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

  nothing _ = Is (Just [])
  everything _ = Is Nothing

  select (Is sel) x = maybe True (\rels -> x `elem` rels) sel

instance Arbitrary Relation where
  arbitrary = oneof $ pure <$> [ FriendsWith
                               , WorksFor
                               ]

instance ToJSON Relation where
  toJSON FriendsWith = String "is friend with"
  toJSON WorksFor    = String "works for"

instance FromJSON Relation where
  parseJSON (String "is friend with") = pure FriendsWith
  parseJSON (String "works for")      = pure WorksFor

instance Selectable BirthPlace where
  data Selector BirthPlace = SelectBirthPlace | NoBirthPlace

  nothing _ = NoBirthPlace
  everything _ = SelectBirthPlace

  select SelectBirthPlace _ = True
  select _ _                = False

instance Arbitrary BirthPlace where
  arbitrary = pure BirthPlace

instance ToJSON BirthPlace where
  toJSON _ = String "is born at"

instance FromJSON BirthPlace where
  parseJSON (String "is born at") = pure BirthPlace


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
selectFriends = EProd (ES Nothing (Is (Just [FriendsWith])) Nothing) (nothing Proxy)

proxy :: Proxy TGraph
proxy = Proxy

instance Named (Character -: Relation :-> Character) where
  name _ = "RELATION"

instance Named (Character -: BirthPlace :-> Location) where
  name _ = "BIRTH"

instance GraphMonad (Character -: Relation :-> Character) Identity where
  selectEdges _ sel = pure $ selects sel relations

instance GraphMonad (Character -: BirthPlace :-> Location) Identity where
  selectEdges _ sel = pure $ selects sel birthPlaces
