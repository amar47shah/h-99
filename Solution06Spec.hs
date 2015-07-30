module Solution06Spec where

import Solution06

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "isPalindrome" $ do
    describe "is true of a list concatenated to its reverse" $ do
      specify "with [Char]" $ property $ (prop :: [Char] -> Property)
      specify "with [Int]"  $ property $ (prop :: [Int]  -> Property)


prop :: Eq a => [a] -> Property
prop xs = property . isPalindrome $ xs ++ reverse xs
