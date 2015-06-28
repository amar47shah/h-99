module Solution25 where

import Solution23 (randomSelect)

randomPermutation :: [a] -> IO [a]
randomPermutation xs = randomSelect xs (length xs)
