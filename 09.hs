pack :: Eq a => [a] -> [[a]]
pack xs = fst $ ack ([], xs)
  where ack (runs, []) = (runs, [])
        ack (runs, xs) = ack (runs ++ [run xs], rest xs)
          where run  (x:xs) = x : takeWhile (== x) xs
                rest (x:xs) =     dropWhile (== x) xs
