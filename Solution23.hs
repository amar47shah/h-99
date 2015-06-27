module Solution23 where

import System.Random (getStdRandom, randomR)
import Data.List (group, sort)

rolls :: Int -> IO [Int]
rolls = (mapM $ getStdRandom . randomR) . (dice 6)

dice :: Int -> Int -> [(Int, Int)]
dice sides n = zip (replicate n 1) (replicate n sides)

--10 rolls of the dice
tenRolls :: IO [Int]
tenRolls = (mapM $ getStdRandom . randomR) $ zip (replicate 10 1) (replicate 10 6)

--results of 100 rolls, grouped by result
resultsOfManyRolls :: Int -> IO [(Int, Int)]
resultsOfManyRolls n = do
  r <- rolls n
  let count = \l -> (head l, length l)
  return . map count . group . sort $ r
