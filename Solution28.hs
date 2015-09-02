module Solution28 (isPrime) where

isPrime :: Integer -> Bool
isPrime p
 | p < 2     = False
 | p == 2    = True
 | otherwise = not $ any (divides p) [2..upperBound p]
   where upperBound n = n `div` 2 + 1

divides :: Integer -> Integer -> Bool
divides q d = q `mod` d == 0
