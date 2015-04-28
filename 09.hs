pack :: Eq a => [a] -> [[a]]
pack xs = fst $ ack ([], xs)
  where ack (runs, [])    = (runs, [])
        ack (runs, xs)    = ack (runs ++ [fst $ runAndRest xs],
                                          snd $ runAndRest xs)
        runAndRest []     = ([], [])
        runAndRest (x:xs) = (x:run, rest)
          where (run, rest) = span (== x) xs
