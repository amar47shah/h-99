module Solution20 where

import Control.Arrow ((&&&))
import Control.Applicative (liftA2)

removeAt :: Int -> [a] -> (a, [a])
removeAt = liftA2 (&&&) (flip (!!) . pred) skipAt

skipAt :: Int -> [a] -> [a]
skipAt i xs = (take . pred) i xs ++ drop i xs

--for good measure
breakAround :: Int -> [a] -> ([a], [a])
breakAround = liftA2 (&&&) (take . pred) drop

--Pointfree experimenting
skipAt', skipAt'', skipAt''' :: Int -> [a] -> [a]
skipAt' i = uncurry (++) . (\n -> liftA2 (&&&) (take . pred) drop n) i
skipAt''  = \i -> uncurry (++) . (\n -> liftA2 (&&&) (take . pred) drop n) i
skipAt''' = \i xs -> (take . pred) i xs ++ drop i xs
