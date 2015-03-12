--99 Problems
--3

elementAt :: [a] -> Int -> a
elementAt [] _   = error "elementAt: empty list"
elementAt l  n 
  | n < 1        = error "elementAt: index too small"
  | n > length l = error "elementAt: index too large" 
  | n == 1       = head l
  | otherwise    = elementAt (tail l) (n - 1) 

elementAt' l n = l !! (n - 1)

elementAt'' [] _         = error "elementAt'': empty list"
elementAt'' (x:_) 1      = x
elementAt'' (_:xs) n
  | n < 1                = error "elementAt'': index too small"
  | n > (1 + length xs)  = error "elementAt'': index too large"
  | otherwise            = elementAt'' xs (n - 1)
