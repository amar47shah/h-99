import Solution09

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map collapse (pack xs)
  where collapse x = (length x, head x)
