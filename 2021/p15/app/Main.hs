module Main where

import System.Environment
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let riskMap = parseInput contents
    let risk = findLowestRisk riskMap
    print risk

parseInput = map (map digitToInt) . lines

findLowestRisk :: [[Int]] -> Int
findLowestRisk = (Map.! (0, 0)) . getMinimalPaths

getMinimalPaths :: [[Int]] -> Map (Int, Int) Int
getMinimalPaths grid =
    let maxRowCol = ((length grid) - 1)
    in fillDiagonal ((2 * maxRowCol) - 1) grid (Map.singleton (maxRowCol, maxRowCol) 0)

fillDiagonal :: Int -> [[Int]] -> Map (Int, Int) Int -> Map (Int, Int) Int
fillDiagonal n grid curr =
    let pts = getDiagonalPoints ((length grid) - 1) n
    in let minRisks = fillMinimalPaths pts grid curr
    in if n == 0 then minRisks else fillDiagonal (n - 1) grid minRisks

getDiagonalPoints :: Int -> Int -> [(Int, Int)]
getDiagonalPoints maxRowCol n =
    let minCoord = max 0 (n - maxRowCol)
    in let maxCoord = n - minCoord
    in [(i, n - i) | i <- [minCoord..maxCoord]]

fillMinimalPaths :: [(Int, Int)] -> [[Int]] -> Map (Int, Int) Int -> Map (Int, Int) Int
fillMinimalPaths [] grid curr = curr
fillMinimalPaths ((row, col):rest) grid curr =
    let nextPts = filter (isValidPoint grid) [(row+1, col), (row, col+1)]
    in let minRisk = minimum $ map (\(r, c) -> ((grid !! r) !! c) + (curr Map.! (r, c))) nextPts
    in fillMinimalPaths rest grid $ Map.insert (row, col) minRisk curr

isValidPoint :: [[Int]] -> (Int, Int) -> Bool
isValidPoint grid (row, col) =
    let maxRowCol = (length grid) - 1
    in row >= 0 && row <= maxRowCol && col >= 0 && col <= maxRowCol

