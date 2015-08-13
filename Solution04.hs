module Solution04 where

myLength, myLength', myLength2, myLength3, myLength4, myLength5 :: [a] -> Int
--tail recursive
myLength = accumLength 0
  where accumLength n []     = n
        accumLength n (_:xs) = accumLength (n + 1) xs

myLength' []     = 0
myLength' (_:xs) = 1 + myLength' xs

myLength2 = (foldl (+) 0) . (map (\_ -> 1))
myLength3 = sum . (map (\_ -> 1))
myLength4 = foldl (\n _ -> n + 1) 0
myLength5 = foldr (\_ -> (+1)) 0
