module Solution26 where

import Data.List (nub, permutations, sort, subsequences)

combinations :: Int -> [a] -> [[a]]
combinations k xs = filter ((k ==) . length) $ subsequences xs

combinations' :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations' k = filterTranspositions . permutationsOfLength k

filterTranspositions :: (Eq a, Ord a) => [[a]] -> [[a]]
filterTranspositions = nub . map sort

permutationsOfLength :: (Eq a, Ord a) => Int -> [a] -> [[a]]
permutationsOfLength k xs
  | k > length xs = []
  | otherwise     = sort . nub . map (take k) . permutations $ xs

combinations'' :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations'' k xs
  | k > length xs = []
  | otherwise     = sort . nub . map (sort . take k) . permutations $ xs
