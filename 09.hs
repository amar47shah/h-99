pack :: Eq a => [a] -> [[a]]
pack xs = firstRun xs : firstRun (afterFirstRun xs) : []

same :: Eq a => [a] -> Bool
same xs = all (== head xs) xs

firstRun :: Eq a => [a] -> [a]
firstRun xs = takeWhile (== head xs) xs

afterFirstRun :: Eq a => [a] -> [a]
afterFirstRun xs = dropWhile (== head xs) xs