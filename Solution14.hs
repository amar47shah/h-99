module Solution14 where

dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x : x : dupli xs

dupli2 :: [a] -> [a]
dupli2 = concat . map (\x -> [x, x])

dupli3 :: [a] -> [a]
dupli3 = concatMap (\x -> [x, x])

dupli4 :: [a] -> [a]
dupli4 = foldr (\x xs -> x : x : xs) []
