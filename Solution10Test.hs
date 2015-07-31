module Solution10Test where

import Solution10

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "decode . encode == (id :: [Int] -> [Int])" $
      \xs -> (xs :: [Int]) == (decode . encode) xs
  , QC.testProperty "decode . encode == (id :: [Char] -> [Char])" $
      \xs -> (xs :: [Char]) == (decode . encode) xs
  ]

decode :: [(Int, a)] -> [a]
decode = concatMap $ uncurry replicate
