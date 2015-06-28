module Solution23 where

import Data.List (group, sort)
import System.Random (getStdRandom, randomR)

import Solution20 (removeAt)

randomSelect :: [a] -> Int -> IO [a]
randomSelect xs n
  | n <  0         = return $ error "number of selections must be greater than zero"
  | n >  length xs = return $ error "number of selections must be fewer than list items"
  | n == 0         = return []
  | otherwise      = do (chosen, remaining) <- randomRemove xs
                        next <- randomSelect remaining $ pred n
                        return $ chosen : next

safeRandomSelect :: [a] -> Int -> IO (Maybe [a])
safeRandomSelect xs n
  | n `elem` [1..length xs] = do rs <- randomSelect xs n
                                 return $ Just rs
  | otherwise               = return Nothing

randomRemove :: [a] -> IO (a, [a])
randomRemove xs = do
  i <- randomPlace xs
  return $ removeAt i xs

randomPlace :: [a] -> IO Int
randomPlace xs = getStdRandom . randomR $ (1, length xs)

--Random Elements
randomElement :: [a] -> IO (Maybe a)
randomElement xs = do
  i <- randomIndex xs
  return $ case i of Nothing -> Nothing
                     Just i  -> Just $ xs !! i

randomIndex :: [a] -> IO (Maybe Int)
randomIndex [] = return Nothing
randomIndex xs = do
  i <- getStdRandom . randomR $ indexRange xs
  return $ Just i

indexRange :: [a] -> (Int, Int)
indexRange = (,) 0 . (pred . length)

--Dice Rolling:
--an investigation into System.Random
rolls :: Int -> IO [Int]
rolls = (mapM $ getStdRandom . randomR) . (dice 6)

dice :: Int -> Int -> [(Int, Int)]
dice sides n = replicate n 1 `zip` replicate n sides

--10 rolls of the dice
tenRolls :: IO [Int]
tenRolls = rolls 10
  --(mapM $ getStdRandom . randomR) $ zip (replicate 10 1) (replicate 10 6)

--results of 100 rolls, grouped by result
resultsOfManyRolls :: Int -> IO [(Int, Int)]
resultsOfManyRolls n = do
  r <- rolls n
  let count = \l -> (head l, length l)
  return . map count . group . sort $ r
