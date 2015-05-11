module Solution02 where

myButLast :: [a] -> a
myButLast [] = error "myButLast: empty list"
myButLast [x] = error "myButLast: too few elements"
myButLast ([x,_]) = x
myButLast (_:xs) = myButLast xs

myButLast' = last . init

myButLast'' = head . tail . reverse
