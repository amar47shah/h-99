module Solution13 where

import Solution11 (Run(..))

encodeDirect :: Eq a => [a] -> [Run Int a]
encodeDirect = foldr step []
  where step x [] = [Single x]
        step x (y@(Single x') : ys)
         | x == x'   = Multiple 2 x' : ys
         | otherwise = Single x : y : ys
        step x (y@(Multiple n x') : ys)
         | x == x'   = Multiple (1 + n) x' : ys
         | otherwise = Single x : y : ys
