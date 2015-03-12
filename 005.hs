--99 Problems
--5

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

--map (\x -> if x == '_' then ' ' else x)
--    (filter (/=' ') (myReverse "a man a p_lan a c_a_nal p_a_nam_a"))

myReverse2 :: [a] -> [a]
myReverse2 list = myReverse2' list []
  where myReverse2' []     reversed = reversed
        myReverse2' (x:xs) reversed = myReverse2' xs (x:reversed)
