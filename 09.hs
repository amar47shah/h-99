pack :: Eq a => [a] -> [[a]]
pack xs = fst $ ack ([], xs)
  where ack  (runs, []) = (runs, [])
        ack  (runs, xs) = ack (runs ++ [run xs], rest xs)
        run  (x:xs)     = x : takeWhile (== x) xs
        run  _          = [] --for completeness
        rest (x:xs)     =     dropWhile (== x) xs
        rest _          = [] --for completeness
