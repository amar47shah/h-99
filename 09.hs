pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (endOfRun, rest) = span (== x) xs
              in  (x:endOfRun) : pack rest
pack _      = []
--  a list of lists of items of type a
--  where the last list is unprocessed ...?
--  [run-one, run-two, ..., unprocessed] <- all items are of type [a]
