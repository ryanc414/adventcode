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
    let lines = parseInput contents
    let count = countHorizontalVerticalLineOverlaps lines
    print count
    let count2 = countAllLineOverlaps lines
    print count2

newtype LineSegment = LineSegment ((Int, Int), (Int, Int)) deriving Show

parseInput :: String -> [LineSegment]
parseInput = (map parseLineSegment) . lines

parseLineSegment :: String -> LineSegment
parseLineSegment str =
    let points = splitOn " -> " str
    in let [a, b] = map parsePoint points
    in LineSegment (a, b)

parsePoint :: String -> (Int, Int)
parsePoint str =
    let [x, y] = map read $ splitOn "," str
    in (x, y)

countHorizontalVerticalLineOverlaps :: [LineSegment] -> Int
countHorizontalVerticalLineOverlaps =
    countOverlapPositions . produceLineDiagram . filterVerticalHorizontal

countAllLineOverlaps :: [LineSegment] -> Int
countAllLineOverlaps = countOverlapPositions . produceLineDiagram

filterVerticalHorizontal :: [LineSegment] -> [LineSegment]
filterVerticalHorizontal = filter isVerticalHorizontal
    where
        isVerticalHorizontal :: LineSegment -> Bool
        isVerticalHorizontal (LineSegment ((x1, y1), (x2, y2))) =
            (x1 == x2) || (y1 == y2)

produceLineDiagram :: [LineSegment] -> Map (Int, Int) Int
produceLineDiagram [] = Map.empty
produceLineDiagram (l:ls) =
    let points = getLinePoints l
        restDiagram = produceLineDiagram ls
    in Map.unionWith (+) (pointsToMap points) restDiagram

getLinePoints :: LineSegment -> [(Int, Int)]
getLinePoints (LineSegment ((x1, y1), (x2, y2))) =
    let (dx, dy) = (normalise (x2 - x1), normalise (y2-y1))
        length = max (abs (x2 - x1)) (abs (y2 - y1))
    in [(x1 + (i * dx), y1 + (i * dy)) | i <- [0..length]]
    where
        normalise :: Int -> Int
        normalise 0 = 0
        normalise x = x `div` (abs x)

getMinMax :: (Int, Int) -> (Int, Int)
getMinMax (a, b) = if b > a then (a, b) else (b, a)

pointsToMap :: [(Int, Int)] -> Map (Int, Int) Int
pointsToMap = Map.fromList . map (\p -> (p, 1))

countOverlapPositions :: Map (Int, Int) Int -> Int
countOverlapPositions = Map.foldr (\count total -> if count > 1 then total + 1 else total) 0

