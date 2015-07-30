module Solution10Spec where

import Solution10

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "encode" $ do
    describe "can be decoded" $ do
      specify "with [Char]" . property $
        (inverts :: [Char] -> Property)
      specify "with [Int]" . property $
        (inverts :: [Int] -> Property)

inverts :: Eq a => [a] -> Property
inverts xs = property $ xs == (decode . encode) xs
  where decode = concatMap $ \(n,e) -> replicate n e
