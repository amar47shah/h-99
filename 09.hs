pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let run  = x : takeWhile (== x) xs
                  rest =     dropWhile (== x) xs
              in  run : pack rest
pack _      = []
--  a list of lists of items of type a
--  where the last list is unprocessed ...?
--  [run-one, run-two, ..., unprocessed] <- all items are of type [a]
