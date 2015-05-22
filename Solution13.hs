module Solution13 where

import Data.List (foldl')
import Solution11 (Run(..))

-- Kinda cheating, since span does create a sublist for each run.
encodeDirect :: Eq a => [a] -> [Run Int a]
encodeDirect = map modify . encode
  where modify (1, x) = Single x
        modify (n, x) = Multiple n x
        encode xs = fst $ foldl' step ([], xs) xs
          where step (codes, xs) x
                  | null run  = (codes                  , rest)
                  | otherwise = (codes ++ [collapse run], rest)
                  where (run, rest) = span (== x) xs
                        collapse xs = (length xs, head xs)
