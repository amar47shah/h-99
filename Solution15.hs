module Solution15 where

repli :: (Ord b, Num b) => [a] -> b -> [a]
repli xs n = concatMap (\x -> multi x n) xs

repli' :: (Ord b, Num b) => [a] -> b -> [a]
repli' xs n = foldr (\x xs -> multi x n ++ xs) [] xs

multi :: (Ord b, Num b) => a -> b -> [a]
multi x n
  | n > 0     = x : multi x (n - 1)
  | otherwise = []
