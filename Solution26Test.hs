module Solution26Test where

import Solution26

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain qcProps

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "length (combinations k xs) == length xs `choose` k" $
      \k xs -> (length (xs :: [Int]) < 15) QC.==>
               length (combinations k xs) == length xs `choose` k
  ]

choose :: Int -> Int -> Int
choose _ 0 = 1
choose 0 _ = 0
choose n k = choose (n-1) (k-1) * n `div` k
