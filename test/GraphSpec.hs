{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GraphSpec where

import           Test.Hspec              (Spec, describe, it, shouldBe)
import           Control.Monad.Identity  (Identity(..))
import           Data.Proxy
import           Web.Ogma.Resource

spec :: Spec
spec = describe "selectEdges" $ do
  it "should select everything" $ do
    selectEdges proxy (everything Proxy) `shouldBe`
      Identity allRes

  it "should select only Relation" $ do
    selectEdges proxy (EProd (everything Proxy) (nothing Proxy)) `shouldBe`
      Identity onlyRelation

  it "should select only FriendsWith" $ do
    selectEdges proxy selectFriends `shouldBe`
      Identity onlyFriends

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

instance Selectable BirthPlace where
  data Selector BirthPlace = SelectBirthPlace | NoBirthPlace

  nothing _ = NoBirthPlace
  everything _ = SelectBirthPlace

  select SelectBirthPlace _ = True
  select _ _ = False

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
  selectEdges _ sel = pure $ filter (select sel) relations

instance GraphMonad (Character -: BirthPlace :-> Location) Identity where
  selectEdges _ sel = pure $ filter (select sel) birthPlaces
