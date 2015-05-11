module Solution11 where

import Solution10

data Run n a = Single a | Multiple n a
               deriving (Show)

encodeModified :: Eq a => [a] -> [Run Int a]
encodeModified = map modify . encode
  where modify (1, x) = Single x
        modify (n, x) = Multiple n x
