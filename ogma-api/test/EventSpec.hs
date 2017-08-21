module EventSpec where

import           Test.Hspec         (Spec, describe, it)
import           Test.QuickCheck    (Arbitrary(arbitrary), property, oneof)

import           Web.Ogma.Api

instance Arbitrary AbsoluteTime where
  arbitrary = intToAbs <$> arbitrary

instance Arbitrary TimeInterval where
  arbitrary = oneof [ boundedTime <$> arbitrary <*> arbitrary
                    , after <$> arbitrary
                    , before <$> arbitrary
                    , union <$> arbitrary <*> arbitrary
                    , exact <$> arbitrary
                    ]

spec :: Spec
spec = do
  describe "overlap" $ do
    it "should be reflexive" $ property $
      \x -> (overlap x x == True)

    it "should be symmetric" $ property $
      \x y -> (overlap x y == overlap y x)
