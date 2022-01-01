module Main where

import System.Environment
import Data.List.Split

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let targetArea = parseTarget contents
    let highestY = findHighestY targetArea
    print highestY
    let count = countInitialVelocities targetArea
    print count

parseTarget :: String -> ((Int, Int), (Int, Int))
parseTarget contents =
    let [[xMin, xMax], [yMin, yMax]] = map (map read . splitOn ",")  (splitOn ";" contents)
    in ((xMin, xMax), (yMin, yMax))

findHighestY :: ((Int, Int), (Int, Int)) -> Int
findHighestY (_, (y1, y2)) =
    let yMin = min y1 y2
    in triNum $ (abs yMin) - 1

triNum :: Int -> Int
triNum n = (n * (n + 1)) `div` 2

countInitialVelocities :: ((Int, Int), (Int, Int)) -> Int
countInitialVelocities = length . getInitialVelocities

getInitialVelocities :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
getInitialVelocities ((x0, x1), (y0, y1)) =
    filter (hitsTarget ((x0, x1), (y0, y1))) [(x, y) | x <- [0..x1], y <- [y0..(-y0 - 1)]]

hitsTarget :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
hitsTarget = hitsTargetFrom (0, 0)
    where
        hitsTargetFrom :: (Int, Int) -> ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
        hitsTargetFrom (x, y) ((x0, x1), (y0, y1)) (vx, vy) =
            if ((x >= x0) && (x <= x1)) && ((y >= y0) && (y <= y1))
            then True
            else if (x > x1) || (y < y0)
                 then False
                 else hitsTargetFrom (x + vx, y + vy) ((x0, x1), (y0, y1)) (incVel (vx, vy))

incVel :: (Int, Int) -> (Int, Int)
incVel (vx, vy) =
    if vx > 0
    then (vx - 1, vy - 1)
    else if vx < 0
         then (vx + 1, vy - 1)
         else (vx, vy - 1)

