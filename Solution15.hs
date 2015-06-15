module Solution15 where

repli :: (Ord b, Num b) => [a] -> b -> [a]
repli xs n = concatMap (\x -> multi x n) xs

repli' :: (Ord b, Num b) => [a] -> b -> [a]
repli' xs n = foldr (\x xs -> multi x n ++ xs) [] xs

multi :: (Ord b, Num b) => a -> b -> [a]
multi x n
  | n > 0     = x : multi x (n - 1)
  | otherwise = []

repli3 :: [a] -> Int -> [a]
repli3 xs n = concatMap (replicate n) xs

repli4 :: [a] -> Int -> [a]
repli4 xs n = foldr (\x xs -> replicate n x ++ xs) [] xs

repli5 :: [a] -> Int -> [a]
repli5 = flip $ concatMap . replicate
