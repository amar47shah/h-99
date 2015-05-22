module Solution13 where

import Solution11 (Run(..))

encodeDirect :: Eq a => [a] -> [Run Int a]
encodeDirect = map modify . encode
  where modify (1, x) = Single x
        modify (n, x) = Multiple n x
        encode = foldr step []
          where step x []             = (    1, x ):[]
                step x (y@(n, x'):ys)
                 | x == x'            = (n + 1, x'):ys
                 | otherwise          = (    1, x ):y:ys
