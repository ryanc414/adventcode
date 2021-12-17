module Main where

import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let heightMap = parseInput contents
    let lowPoints = getLowPoints heightMap
    let risk = sumRiskLevels heightMap lowPoints
    print risk
    let basinMultiple = multiplyLargestBasinSizes heightMap lowPoints
    print basinMultiple

parseInput :: String -> [[Int]]
parseInput = map (map digitToInt) . lines

sumRiskLevels :: [[Int]] -> [(Int, Int)] -> Int
sumRiskLevels heightMap = foldr (+) 0 . map (getRiskLevel . getHeight heightMap)

getRiskLevel :: Int -> Int
getRiskLevel height = 1 + height

getHeight :: [[Int]] -> (Int, Int) -> Int
getHeight heightMap (row, col) = (heightMap !! row) !! col

getLowPoints :: [[Int]] -> [(Int,Int)]
getLowPoints heightMap =
    let maxRow = (length heightMap) - 1
        maxCol = (length (head heightMap)) - 1
    in filter (isLowPoint heightMap) [(row, col) | row <- [0..maxRow], col <- [0..maxCol]]

isLowPoint :: [[Int]] -> (Int, Int) -> Bool
isLowPoint heightMap (row, col) =
    let height = (heightMap !! row) !! col
    in all (height <) (getNeighbourHeights heightMap (row, col))

getNeighbourHeights :: [[Int]] -> (Int, Int) -> [Int]
getNeighbourHeights heightMap (row, col) =
    map (getHeight heightMap) $ getNeighbourPoints heightMap (row, col)

getNeighbourPoints :: [[Int]] -> (Int, Int) -> [(Int, Int)]
getNeighbourPoints heightMap point = filter (isValidPoint heightMap) $ genAllNeighbours point

genAllNeighbours :: (Int, Int) -> [(Int, Int)]
genAllNeighbours (row, col) =
    [(row+1, col), (row, col+1), (row-1, col), (row, col-1)]

isValidPoint :: [[Int]] -> (Int, Int) -> Bool
isValidPoint heightMap (row, col) =
    let rowLen = (length heightMap)
        colLen = (length (head heightMap))
    in row >= 0 && row < rowLen && col >= 0 && col < colLen

multiplyLargestBasinSizes :: [[Int]] -> [(Int, Int)] -> Int
multiplyLargestBasinSizes heightMap =
    foldr (*) 1 . take 3 . reverse . sort . getAllBasinSizes heightMap

getAllBasinSizes :: [[Int]] -> [(Int, Int)] -> [Int]
getAllBasinSizes heightMap = map (getBasinSize heightMap)

getBasinSize :: [[Int]] -> (Int, Int) -> Int
getBasinSize heightMap lowPoint = Set.size $ explore heightMap lowPoint (Set.empty)
    where
        explore :: [[Int]] -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
        explore heightMap point visited =
            let neighbours = getNeighbourPoints heightMap point
                currHeight = getHeight heightMap point
                newVisited = Set.insert point visited
            in let toExplore = filter (shouldExplore heightMap visited currHeight) neighbours
            in foldr (explore heightMap) newVisited toExplore

shouldExplore :: [[Int]] -> Set (Int, Int) -> Int -> (Int, Int) -> Bool
shouldExplore heightMap visited currHeight nextPoint =
    let alreadyExplored = Set.member nextPoint visited
        height = getHeight heightMap nextPoint
    in (not alreadyExplored) && (height < 9 && height > currHeight)

