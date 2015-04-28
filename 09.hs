pack :: Eq a => [a] -> [[a]]
pack xs = fst $ ack ([], xs)
  where ack (runs, []) = (runs, [])
        ack (runs, xs) = ack (runs ++ [run xs], rest xs)

run  :: Eq a => [a] -> [a]
run  (x:xs) = x : takeWhile (== x) xs
run  []     = []

rest :: Eq a => [a] -> [a]
rest (x:xs) =     dropWhile (== x) xs
rest []     = []
