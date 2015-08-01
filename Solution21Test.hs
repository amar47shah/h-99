module Solution21Test where

import Solution21

import Data.List (sort)

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain qcProps

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "length (insertAt x xs i) = 1 + length xs" $
      \x xs i -> length (insertAt (x :: Char) (xs :: [Char]) i) == 1 + length xs
  , QC.testProperty "sort (insertAt x xs i) = sort (x : xs)" $
      \x xs i -> sort (insertAt (x :: Char) (xs :: [Char]) i) == sort (x : xs)
  ]
