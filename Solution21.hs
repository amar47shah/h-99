module Solution21 where

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = take j xs ++ x : drop j xs
    where j = pred i
