module Solution17 where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))

split , split2, split3, split4         :: [a] -> Int -> ([a], [a])
split5, split6, split7, split8, split9 :: [a] -> Int -> ([a], [a])

split xs n = (up_to xs n, after xs n)
    where up_to _ n' | n' <= 0 = []
          up_to [] _ = []
          up_to (x:xs') n' = x : up_to xs' (n' - 1)
                          -- (:) x . up_to xs' $ pred n'
          after xs' n' | n' <= 0 = xs'
          after [] _ = []
          after (_:xs') n' = after xs' (n' - 1)
                          -- after xs' $ pred n'

split2 xs n | n <= 0 = ([], xs)
split2 [] _         = ([], [])
split2 (x:xs) n     = (x : fst recurse, snd recurse)
    where recurse = split2 xs $ n - 1

split3 = flip splitAt
split4 xs n = (take n xs, drop n xs)
split5 xs n = (take n &&& drop n) xs
split6 xs n = liftA2 (&&&) take drop n xs
split7 = flip $ liftA2 (&&&) take drop
split8 = flip $ liftA2 (&&&) myTake myDrop
split9 = flip mySplitAt

myTake :: Int -> [a] -> [a]
myTake n _      | n <= 0 = []
myTake _ []              = []
myTake n (x:xs)          = x : (myTake . pred) n xs

myDrop :: Int -> [a] -> [a]
myDrop n xs     | n <= 0 = xs
myDrop _ []              = []
myDrop n (_:xs)          =     (myDrop . pred) n xs

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt = liftA2 (&&&) myTake myDrop
