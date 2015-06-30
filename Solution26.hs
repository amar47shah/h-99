module Solution26 where

import Solution20 (removeAt)

combinations :: Int -> [a] -> [[a]]
combinations = undefined

chooseZero :: [a] -> [[a]]
chooseZero xs = []

chooseOne :: [a] -> [[a]]
chooseOne xs = map (\i -> (fst $ removeAt i xs) : []) [1..length xs]
               --map (\x -> [x]) xs
