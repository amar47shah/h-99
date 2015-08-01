module Solution22Test where

import Solution22

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain qcProps

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "range x y == [x..y]" $ \x y -> range x y == [x..y]
  ]
