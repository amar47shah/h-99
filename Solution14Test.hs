module Solution14Test where

import Solution14

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain qcProps

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "dropEveryOther . dupli == (id :: [Int] -> [Int])" $
      \xs -> (xs :: [Int]) == (dropEveryOther . dupli) xs
  , QC.testProperty "dropEveryOther . dupli == (id :: [Char] -> [Char])" $
      \xs -> (xs :: [Char]) == (dropEveryOther . dupli) xs
  ]

dropEveryOther :: [a] -> [a]
dropEveryOther (x:_:xs) = x : dropEveryOther xs
dropEveryOther xs = xs
