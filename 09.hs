pack :: Eq a => [a] -> [[a]]
pack xs = fst $ ack ([], xs)
  where ack (runs, []) = (runs, [])
        ack (runs, xs) = ack (runs ++ [fst $ runAndRest xs],
                                       snd $ runAndRest xs)

runAndRest :: Eq a => [a] -> ([a], [a])
runAndRest (x:xs) = let (  first, second) = span (== x) xs
                    in  (x:first, second)
runAndRest _      = ([], [])
