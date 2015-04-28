pack :: Eq a => [a] -> [[a]]
pack xs = let run  (x:xs) = x : takeWhile (== x) xs
              rest (x:xs) =     dropWhile (== x) xs
          in  let ack (runs, []) = (runs, [])
                  ack (runs, xs) = ack (runs ++ [run xs], rest xs)
              in  fst $ ack ([], xs)
--still recursing on ack, our accumulator
--ack is a 2-tuple with two pieces:
--  the runs found so far
--      list of lists of items of type a
--      processed piece
--  the remaining items
--      list of items of type a
--      unprocessed piece
--
--do we really need to pass around two pieces?
--
--why not:
--  a list of lists of items of type a
--  where the last list is unprocessed ...?
--  [run-one, run-two, ..., unprocessed] <- all items are of type [a]
