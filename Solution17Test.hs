module Solution17Test where

import Solution17

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain qcProps

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "xs == uncurry (++) (split xs n)" $
      \xs n -> (xs :: [Char]) == uncurry (++) (split xs n)
  , QC.testProperty "n == (length . fst) (split xs n)"  $
      \xs n -> (0 <= n && n <= length xs) ==>
        n == (length . fst) (split (xs :: [Char]) n)
  ]
