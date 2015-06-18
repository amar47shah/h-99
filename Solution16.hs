module Solution16 where

dropEvery :: Int -> [a] -> [a]
dropEvery n = concatMap (take $ pred n) . chunksOf n

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | n > 0     = take n xs : (chunksOf n $ drop n xs)
  | otherwise = []
