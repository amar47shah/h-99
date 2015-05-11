module Solution04 where

myLength = length

myLength' :: [a] -> Int
myLength' = accumLength 0
  where accumLength n []     = n
        accumLength n (_:xs) = accumLength (n + 1) xs

myLength'' :: [a] -> Int
myLength'' []     = 0
myLength'' (_:xs) = 1 + myLength'' xs

myLength3 :: [a] -> Int
myLength3 = (foldl (+) 0) . (map (\_ -> 1))

myLength4 = sum . (map (\_ -> 1))

myLength5 = foldl (\n _ -> n + 1) 0
myLength6 = foldr (\_ -> (+1)) 0
