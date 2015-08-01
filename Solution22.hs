module Solution22 where

range :: Int -> Int -> [Int]
range x y
 | x > y     = []
 | otherwise = x : range (succ x) y

range' :: Int -> Int -> [Int]
range' x y = [x..y]
