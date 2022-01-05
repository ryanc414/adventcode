module Main where

import System.Environment
import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let scannerInput = parseScanners contents
    let (beacons, scanners) = getAbsoluteBeacons scannerInput
    print $ Set.size beacons
    let maxDist = findMaxManhattanDistance $ Set.toList scanners
    print maxDist

parseScanners :: String -> [[(Int, Int, Int)]]
parseScanners = map parsePoints . splitOn "\n\n"

parsePoints :: String -> [(Int, Int, Int)]
parsePoints = map parsePoint . tail . lines

parsePoint :: String -> (Int, Int, Int)
parsePoint line =
    let [x, y, z] = (map read . splitOn ",") line
    in (x, y, z)

getAbsoluteBeacons :: [[(Int, Int, Int)]] -> (Set (Int, Int, Int), Set (Int, Int, Int))
getAbsoluteBeacons scannersInput =
    let Just (h, t) = uncons scannersInput
    in findNextBeacons (Set.fromList h, Set.singleton (0, 0, 0)) t

findNextBeacons :: (Set (Int, Int, Int), Set (Int, Int, Int)) -> [[(Int, Int, Int)]] -> (Set (Int, Int, Int), Set (Int, Int, Int))
findNextBeacons state [] = state
findNextBeacons (absolutes, scanners) (next:rest) =
    case findAbsoluteBeacons absolutes next
    of Just (newAbsolutes, newScanner) ->
        let updatedAbsolutes = Set.union absolutes newAbsolutes
            updatedScanners = Set.insert newScanner scanners
        in findNextBeacons (updatedAbsolutes, updatedScanners) rest
       Nothing -> findNextBeacons (absolutes, scanners) (rest ++ [next])

findAbsoluteBeacons :: Set (Int, Int, Int) -> [(Int, Int, Int)] -> Maybe (Set (Int, Int, Int), (Int, Int, Int))
findAbsoluteBeacons absolutes relatives =
    let orientations = [(v, f, i, j) | v <- views, f <- [0..5], i <- [0..(length absolutes) ], j <- [0..(length relatives)]]
    in let found = findAbsoluteBeaconsFrom orientations (Set.toList absolutes) relatives
    in found >>= \(abs, scanner) -> Just $ (Set.fromList abs, scanner)

findAbsoluteBeaconsFrom :: [((Int, Int, Int), Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> Maybe ([(Int, Int, Int)], (Int, Int, Int))
findAbsoluteBeaconsFrom [] _ _ = Nothing
findAbsoluteBeaconsFrom (x:xs) absolutes relatives =
    case findBeaconsFromParams x absolutes relatives
    of Just result -> Just result
       Nothing -> findAbsoluteBeaconsFrom xs absolutes relatives

findBeaconsFromParams :: ((Int, Int, Int), Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> Maybe ([(Int, Int, Int)], (Int, Int, Int))
findBeaconsFromParams (v, f, i, j) absolutes relatives =
    let reorderedAbsolutes = cutList i absolutes
        reorderedRelatives = cutList j relatives
    in let reorientedRelatives = map (applyOrientation (v, f)) reorderedRelatives
    in let displacement = psub (head reorderedAbsolutes) (head reorientedRelatives)
    in let rebasedRelatives = map (psub displacement) reorientedRelatives
    in let overlap = Set.intersection (Set.fromList reorderedAbsolutes) (Set.fromList rebasedRelatives)
    in if Set.size overlap >= 12 then Just (rebasedRelatives, psub displacement (0, 0, 0))
    else Nothing

cutList :: Int -> [a] -> [a]
cutList n l =
    let (a, b) = splitAt n l
    in b ++ a

views = [(1, 1, 1), (1, 1, -1), (1, -1, 1), (1, -1, -1),
         (-1, 1, 1), (-1, 1, -1), (-1, -1, 1), (-1, -1, -1)]

facings = [0..5]

applyOrientation :: ((Int, Int, Int), Int) -> (Int, Int, Int) -> (Int, Int, Int)
applyOrientation (v, f) = (applyView v) . (applyFacing f)

applyView :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
applyView (vx, vy, vz) (x, y, z) = (vx * x, vy * y, vz * z)

applyFacing :: Int -> (Int, Int, Int) -> (Int, Int, Int)
applyFacing i (x, y, z) =
    case i
    of 0 -> (x, y, z)
       1 -> (x, z, y)
       2 -> (y, x, z)
       3 -> (y, z, x)
       4 -> (z, x, y)
       5 -> (z, y, x)

psub :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
psub (x0, y0, z0) (x1, y1, z1) = (x1 - x0, y1 - y0, z1 - z0)

findMaxManhattanDistance :: [(Int, Int, Int)] -> Int
findMaxManhattanDistance scanners = maximum $ map getManhattanDistance [(a, b) | a <- scanners, b <- scanners, a /= b]

getManhattanDistance :: ((Int, Int, Int), (Int, Int, Int)) -> Int
getManhattanDistance ((x0, y0, z0), (x1, y1, z1)) =
    (abs (x1 - x0)) + (abs (y1 - y0)) + (abs (z1 - z0))

