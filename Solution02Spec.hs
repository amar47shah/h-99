module Solution02Spec where

import Solution02

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "myButLast" $ do
    describe "equals last . init" $ do
      specify "with [Char]" $ property $ (prop :: [Char] -> Property)
      specify "with [Int]" $ property $ (prop :: [Int] -> Property)

prop :: Eq a => [a] -> Property
prop xs = (length xs > 1) ==> myButLast xs == (last . init) xs
