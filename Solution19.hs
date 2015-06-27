module Solution19 where

import Data.Tuple (swap)

rotate :: [a] -> Int -> [a]
rotate xs n = uncurry (++) . swap $ splitAt break xs
    where break | n >= 0 = n
                | n <  0 = length xs + n

--slightly more interesting function
rotate' :: [a] -> Int -> [a]
rotate' xs n = uncurry (++) . swap $ splitAt break xs
    where break = n `mod` length xs
