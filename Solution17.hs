module Solution17 where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))

split, split2, split3, split4, split5, split6, split7 :: [a] -> Int -> ([a], [a])

split xs n = (first xs n, after xs n)
    where first _      n | n <= 0 = []
          first []     _          = []
          first (x:xs) n          = x : first xs (n - 1)
                                    --pointfree? (:) x . first xs $ pred n
          after xs     n | n <= 0 = xs
          after []     _          = []
          after (x:xs) n          = after xs $ pred n

split2 xs     n | n <= 0 = ([], xs)
split2 []     _          = ([], [])
split2 (x:xs) n          = (x : fst recurse, snd recurse)
  where recurse = split2 xs $ n - 1

split3 = flip splitAt
split4 xs n = (take n xs, drop n xs)
split5 xs n = (take n &&& drop n) xs
split6 xs n = liftA2 (&&&) take drop n xs
split7 = flip $ liftA2 (&&&) take drop
