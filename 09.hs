pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (run, rest) = span (== x) xs
              in  (x:run) : pack rest
pack _      = []
