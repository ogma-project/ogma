module DataSpec where

import           Data.Aeson
import           Data.Text       (Text, pack)
import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (Arbitrary (arbitrary), oneof, property)

import           Web.Ogma.Data

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary AbsoluteTime where
  arbitrary = intToAbs <$> arbitrary

instance Arbitrary TimeInterval where
  arbitrary = oneof [ boundedTime <$> arbitrary <*> arbitrary
                    , after <$> arbitrary
                    , before <$> arbitrary
                    , union <$> arbitrary <*> arbitrary
                    , exact <$> arbitrary
                    ]

instance Arbitrary Point where
  arbitrary = Point <$> arbitrary <*> arbitrary

instance Arbitrary Surface where
  arbitrary = oneof [ circle <$> arbitrary <*> arbitrary
                    , polygon <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    ]

instance Arbitrary Location where
  arbitrary = Location <$> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "overlap" $ do
    it "should be reflexive" $ property $
      \x -> (overlap x x == True)

    it "should be symmetric" $ property $
      \x y -> (overlap x y == overlap y x)

  describe "union" $ do
    it "should be associative" $ property $
      \a x y z -> (overlap a (union x (union y z)) == overlap a (union (union x y) z))

    it "should be symmetric" $ property $
      \a x y -> (overlap a (union x y) == overlap a (union x y))

  describe "readInterval" $ do
    it "should be the inverse of show" $ property $
      \x -> (readInterval . show) x == Just x

  describe "Aeson instances (TimeInterval)" $ do
    it "should be inverse functions of each other" $ property $
      \x -> (fromJSON . toJSON) x == Success (x :: TimeInterval)

  describe "readSurface" $ do
    it "should be the inverse of show" $ property $
      \x -> (readSurface . show) x == Just x

  describe "collide" $ do
    it "should be reflexive" $ property $
      \x -> (collide x x == True)

    it "should be symmetric" $ property $
      \x y -> (collide x y == collide y x)

  describe "Aeson instances (Surface)" $ do
    it "should be inverse functions of each other" $ property $
      \x -> (fromJSON . toJSON) x == Success (x :: Surface)

  describe "Aeson instances (Location)" $ do
    it "should be inverse functions of each other" $ property $
      \x -> (fromJSON . toJSON) x == Success (x :: Location)
