module Solution15Test where

import Solution15

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain qcProps

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "(n > 0) ==> (xs :: [Int]) == keepFirstOf n (repli xs n)" $
      \n xs -> (n > 0) ==> (xs :: [Int]) == keepFirstOf n (repli xs n)
  , QC.testProperty "(n > 0) ==> (xs :: [Char]) == keepFirstOf n (repli xs n)" $
      \n xs -> (n > 0) ==> (xs :: [Char]) == keepFirstOf n (repli xs n)
  ]

keepFirstOf :: Int -> [a] -> [a]
keepFirstOf n (x:xs) = x : keepFirstOf n (drop (n - 1) xs)
keepFirstOf _ xs = xs
