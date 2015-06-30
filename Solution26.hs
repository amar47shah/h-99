module Solution26 where

import Data.List (nub, permutations, sort, subsequences)

combinations :: Int -> [a] -> [[a]]
combinations n xs = filter ((n ==) . length) $ subsequences xs

combinations' :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations' n = filterTranspositions . permutationsOfLength n

filterTranspositions :: (Eq a, Ord a) => [[a]] -> [[a]]
filterTranspositions = nub . map sort

permutationsOfLength :: (Eq a, Ord a) => Int -> [a] -> [[a]]
permutationsOfLength n xs
  | n > length xs = []
  | otherwise     = sort . nub . map (take n) . permutations $ xs

combinations'' :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations'' n xs
  | n > length xs = []
  | otherwise     = sort . nub . map (sort . take n) . permutations $ xs
