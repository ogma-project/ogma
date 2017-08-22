module Shared where

import           Test.Hspec             (Spec, describe, it, shouldBe)
import           Test.QuickCheck        (Arbitrary (arbitrary), oneof, property, Property)
import Data.Aeson
import Data.Proxy

(-->) :: Bool -> Bool -> Bool
x --> y = not x || y

reflexivity :: (Arbitrary a, Eq a, Show a) => (a -> a -> Bool) -> Property
reflexivity f = property $ \x -> f x x

symmetry :: (Arbitrary a, Eq a, Show a) => (a -> a -> Bool) -> Property
symmetry f = property $ \x y -> f x y --> f y x

transitivy :: (Arbitrary a, Eq a, Show a) => (a -> a -> Bool) -> Property
transitivy f =  property $ \x y z -> f x y --> f y z --> f x z

checkRelation :: (Arbitrary a, Eq a, Show a) => String -> (a -> a -> Bool) -> Spec
checkRelation name f = describe (name ++ " is a relation") $ do
  it "should be reflexive" $ reflexivity f
  it "should be symmetric" $ symmetry f
  it "should be transitive" $ reflexivity f
