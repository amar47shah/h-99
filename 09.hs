pack :: Eq a => [a] -> [[a]]
pack xs = fst $ ack ([], xs)
  where ack (runs, []) = (runs, [])
        ack (runs, xs) = ack (runs ++ [run xs], rest xs)

run  :: Eq a => [a] -> [a]
run  (x:xs) = x : (fst $ span (== x) xs)
run  _      = []

rest :: Eq a => [a] -> [a]
rest (x:xs) =      snd $ span (== x) xs
rest _      = []
