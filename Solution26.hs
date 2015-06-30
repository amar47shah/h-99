module Solution26 where

import Data.List (nub, permutations, sort)

import Solution20 (removeAt)

combinations :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations n xs
  | n > length xs = []
  | otherwise     = sort . nub . map (sort . take n) . permutations $ xs

chooseZero :: [a] -> [[a]]
chooseZero xs = []

chooseOne :: [a] -> [[a]]
chooseOne xs = map (\i -> (fst $ removeAt i xs) : []) [1..length xs]
               --map (\x -> [x]) xs
