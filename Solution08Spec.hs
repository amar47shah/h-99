module Solution08Spec where

import Solution08

import Test.Hspec
import Test.QuickCheck

import Data.List (nub)

main :: IO ()
main = hspec $ do
  describe "compress" $ do
    context "with [Char]" $ do
      it "does not increase length" $ property $
        (doesNotLengthen :: [Char] -> Property)
      it "preserves list of unique elements" $ property $
        (preservesUniqueList :: [Char] -> Property)
      it "has no effect on an already-compressed list" $ property $
        (noEffectWhenRepeated :: [Char] -> Property)
    context "with [Int]" $ do
      it "does not increase length" $ property $
        (doesNotLengthen :: [Int] -> Property)
      it "preserves list of unique elements" $ property $
        (preservesUniqueList :: [Int] -> Property)
      it "has no effect on an already-compressed list" $ property $
        (noEffectWhenRepeated :: [Int] -> Property)


doesNotLengthen :: Eq a => [a] -> Property
doesNotLengthen xs = property $ length (compress xs) <= length xs

preservesUniqueList :: Eq a => [a] -> Property
preservesUniqueList xs = property $ nub (compress xs) == nub xs

noEffectWhenRepeated :: Eq a => [a] -> Property
noEffectWhenRepeated xs = property $ compress (compress xs) == compress xs
