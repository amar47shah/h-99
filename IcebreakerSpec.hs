module IcebreakerSpec where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "add" $ do
    it "is commutative" $ property $
      \x y -> add x y == add y x

add :: Int -> Int -> Int
add x y = x + y
