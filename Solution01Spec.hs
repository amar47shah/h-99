module Solution01Spec where

import Solution01

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "myLast" $ do
    describe "complements init" $ do
      specify "with [Char]" $ property $ (prop :: [Char] -> Property)
      specify "with [Int]" $ property $ (prop :: [Int] -> Property)

prop :: Eq a => [a] -> Property
prop x = (not $ null x) ==> x == init x ++ [myLast x]
