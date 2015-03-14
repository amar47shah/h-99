compress :: Eq a => [a] -> [a]
compress (x:y:xs)
 | x == y         =        compress (x:xs)
 | x /= y         = [x] ++ compress (y:xs)
compress (x:[])   = [x]
compress []       = []
