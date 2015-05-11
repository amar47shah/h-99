module Solution10 where

import Solution09

encode :: Eq a => [a] -> [(Int, a)]
encode = map collapse . pack
  where collapse x = (length x, head x)
