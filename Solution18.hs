module Solution18 where

slice :: [a] -> Int -> Int -> [a]
slice xs i j = take (j - i + 1) $ drop (i - 1) xs
