module Solution11 where

import Solution10

data Run n a = Single a | Multiple n a
               deriving (Show)

modify :: (Int, a) -> Run Int a
modify (1, x) = Single x
modify (n, x) = Multiple n x

encodeModified :: Eq a => [a] -> [Run Int a]
encodeModified xs = map modify (encode xs)
