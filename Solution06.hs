module Solution06 where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = l == (reverse l)
