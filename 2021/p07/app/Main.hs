module Main where

import System.Environment
import Data.List
import Data.List.Split

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let initialPositions = parseInput contents
    let fuel = calculateMinFuel initialPositions
    print fuel
    let fuel2 = calculateMinFuel2 initialPositions
    print fuel2

parseInput :: String -> [Int]
parseInput = (map read) . (splitOn ",")

calculateMinFuel :: [Int] -> Int
calculateMinFuel positions = calculateTotalFuel positions $ getMedian positions

calculateMinFuel2 :: [Int] -> Int
calculateMinFuel2 positions =
    foldr1 min $ calculateAllTotalFuel positions

calculateTotalFuel :: [Int] -> Int -> Int
calculateTotalFuel positions pos = foldr (+) 0 $ map (\p -> abs (p - pos)) positions

calculateAllTotalFuel :: [Int] -> [Int]
calculateAllTotalFuel positions =
    let minPos = foldr1 min positions
        maxPos = foldr1 max positions
    in map (calculateTotalFuel2 positions) [minPos..maxPos]

calculateTotalFuel2 :: [Int] -> Int -> Int
calculateTotalFuel2 positions pos = foldr (+) 0 $ map (\p -> triangleNum $ abs (p - pos)) positions

triangleNum :: Int -> Int
triangleNum n = (n * (n + 1)) `div` 2

getMedian :: [Int] -> Int
getMedian xs =
    let i = (length xs) `div` 2
    in (sort xs) !! i

