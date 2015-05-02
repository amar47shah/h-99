module Solution09 where

import Data.List (foldl')

pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (run, rest) = span (== x) xs
              in  (x:run) : pack rest
pack _      = []

pack' :: Eq a => [a] -> [[a]]
pack' xs = fst $ foldl' step ([], xs) xs
  where step (runs, xs) x
          | null run  = (runs         , rest)
          | otherwise = (runs ++ [run], rest)
          where (run, rest) = span (== x) xs
