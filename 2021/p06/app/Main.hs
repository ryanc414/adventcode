module Main where

import System.Environment
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let initialFish = parseInput contents
    let numFish = countFishAfter 80 initialFish
    print numFish
    let numFish2 = countFishAfter 256 initialFish
    print numFish2

parseInput :: String -> Map Int Integer
parseInput = numsToMap . (map read) . (splitOn ",")
    where
        numsToMap :: [Int] -> Map Int Integer
        numsToMap [] = Map.empty
        numsToMap (x:xs) =
            let rest = numsToMap xs
                curr = Map.singleton x 1
            in Map.unionWith (+) rest curr

countFishAfter :: Int -> Map Int Integer -> Integer
countFishAfter n = (Map.foldr (+) 0) . (simulateSteps n)

simulateSteps :: Int -> Map Int Integer -> Map Int Integer
simulateSteps 0 fish = fish
simulateSteps n fish = simulateSteps (n - 1) (simulateStep fish)

simulateStep :: Map Int Integer -> Map Int Integer
simulateStep = (Map.foldr (Map.unionWith (+)) Map.empty) . (Map.mapWithKey stepFish)

stepFish :: Int -> Integer -> Map Int Integer
stepFish 0 n = Map.fromList [(6, n), (8, n)]
stepFish m n = Map.singleton (m - 1) n

