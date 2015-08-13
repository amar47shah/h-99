module Solution04Test where

import Solution04

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain qcProps

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "myLength xs == length xs" $
      \xs -> myLength (xs :: [Int]) == length xs
  ]
