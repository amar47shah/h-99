module Solution03Spec where

import Solution03

import Test.Hspec
import Test.QuickCheck


main :: IO ()
main = hspec $ do
  describe "elementAt" $ do
    context "with list of Char" $ do
      context "and 1" $ do
        it "returns the first element" $ property $
          (propHead :: [Char] -> Property)
      context "and the length of the list" $ do
        it "returns the last element" $ property $
          (propLast :: [Char] -> Property)
      context "and any valid 1-index" $
        it "returns the element at the index" $ property $
          (propLookup :: [Int] -> Int -> Property)
    context "with list of Int" $ do
      context "and 1" $ do
        it "returns the first element" $ property $
          (propHead :: [Int] -> Property)
      context "and the length of the list" $ do
        it "returns the last element" $ property $
          (propLast :: [Int] -> Property)
      context "and any valid 1-index" $
        it "returns the element at the index" $ property $
          (propLookup :: [Int] -> Int -> Property)

propHead :: Eq a => [a] -> Property
propHead xs = (length xs > 1) ==> elementAt xs 1 == head xs

propLast :: Eq a => [a] -> Property
propLast xs = (length xs > 1) ==> elementAt xs (length xs) == last xs

propLookup :: Eq a => [a] -> Int -> Property
propLookup xs n = (1 <= n && n <= length xs) ==> elementAt xs n == xs !! (n - 1)
