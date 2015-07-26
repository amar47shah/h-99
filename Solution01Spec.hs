module Solution01Spec where

import Solution01

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "myLast" $ do
    context "of a list of Char" $ do
      it "complements init" $ property $
        ((\x -> (not $ null x) ==> x == init x ++ [myLast x]) :: [Char] -> Property)
    context "of a list of Int" $ do
      it "complements init" $ property $
        ((\x -> (not $ null x) ==> x == init x ++ [myLast x]) :: [Int] -> Property)
