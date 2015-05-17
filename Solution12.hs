module Solution12 where

import Solution11

decodeModified :: [Run Int a] -> [a]
decodeModified = concatMap decode
  where decode (Single     x) = [x]
        decode (Multiple n x)
          | n > 0             = replicate n x
          | otherwise         = []
