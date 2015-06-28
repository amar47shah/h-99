module Solution24 where

import Control.Monad (replicateM)
import System.Random (Random, getStdRandom, randomR)

randomSelect :: Int -> Int -> IO [Int]
randomSelect n high = replicateM n . randomInRange $ (1, high)

randomInRange :: Random a => (a, a) -> IO a
randomInRange = getStdRandom . randomR

--(almost) pointfree
randomSelect' :: Int -> Int -> IO [Int]
randomSelect' n = replicateM n . randomInRange . (,) 1
