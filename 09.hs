pack :: Eq a => [a] -> [[a]]
pack xs = fst $ ack ([], xs)
  where ack (runs, []) = (runs, [])
        ack (runs, xs) = ack (runs ++ [run xs], rest xs)

run :: Eq a => [a] -> [a]
run xs = takeWhile (== head xs) xs

rest :: Eq a => [a] -> [a]
rest xs = dropWhile (== head xs) xs
