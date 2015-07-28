module Solution05Spec where

import Solution05

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "myReverse" $ do
    describe "inverts itself" $ do
      specify "with [Char]" $ property $ (prop :: [Char] -> Property)
      specify "with [Int]"  $ property $ (prop :: [Int]  -> Property)


prop :: Eq a => [a] -> Property
prop xs = property $ xs == (myReverse . myReverse) xs
