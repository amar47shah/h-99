module Solution08 where

compress :: Eq a => [a] -> [a]
compress (x:y:xs)
 | x == y    =     compress (x:xs)
 | otherwise = x : compress (y:xs)
compress xs  = xs
