module Solution20Test where

import Solution20
import Solution21 (insertAt)

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain qcProps

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "xs == (uncurry insertAt) (removeAt k xs) k" $
      \xs k -> (1 <= k && k <= length (xs :: [Char])) ==>
               xs == (uncurry insertAt) (removeAt k xs) k
  ]
