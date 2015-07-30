module Solution09Spec where

import Solution09

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "pack" $ do
    describe "concat . pack == id" $ do
      specify "with [Char]" . property $
        (invertsWithConcat :: [Char] -> Property)
      specify "with [Int]" . property $
        (invertsWithConcat :: [Char] -> Property)

invertsWithConcat :: Eq a => [a] -> Property
invertsWithConcat xs = property $ xs == (concat . pack) xs
