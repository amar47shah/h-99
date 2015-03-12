--Ninety-nine Haskell Problems
--Problem 1

myLast = last

myLast' = head . reverse

myLast'' :: [a] -> a
myLast'' [x] = x
myLast'' (_:xs) = myLast'' xs
myLast'' [] = error "myLast'': empty list"
