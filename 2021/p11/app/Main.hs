module Main where

import System.Environment
import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let energyLevels = parseInput contents
    let count = countFlashesAfterSteps 100 energyLevels
    print count
    let step = findStepAllFlash energyLevels
    print step

parseInput :: String -> [[Int]]
parseInput = map (map digitToInt) . lines

showEnergies :: [[Int]] -> String
showEnergies energyLevels =
    intercalate "\n" (map (map intToDigit) energyLevels)

countFlashesAfterSteps :: Int -> [[Int]] -> Int
countFlashesAfterSteps 0 energyLevels = 0
countFlashesAfterSteps n energyLevels =
    let (newEnergyLevels, flashCount) = stepEnergies energyLevels
    in flashCount + (countFlashesAfterSteps (n - 1) newEnergyLevels)

stepEnergies :: [[Int]] -> ([[Int]], Int)
stepEnergies energyLevels =
    let incremented = map (map (+ 1)) energyLevels
    in processFlashes incremented

findStepAllFlash :: [[Int]] -> Int
findStepAllFlash energyLevels =
    let (newEnergyLevels, flashCount) = stepEnergies energyLevels
        numOctopuses = (length energyLevels) * (length (head energyLevels))
    in if flashCount == numOctopuses then 1
    else 1 + (findStepAllFlash newEnergyLevels)

processFlashes :: [[Int]] -> ([[Int]], Int)
processFlashes energyLevels =
    let (increasedEnergies, flashCoords) = applyFlashIncreases Set.empty energyLevels
    in (resetFlashed increasedEnergies flashCoords, Set.size flashCoords)

applyFlashIncreases :: Set (Int, Int) -> [[Int]] -> ([[Int]], Set (Int, Int))
applyFlashIncreases alreadyFlashed energyLevels =
    let flashCoords = getFlashPoints alreadyFlashed energyLevels
    in if null flashCoords then (energyLevels, alreadyFlashed)
    else let newEnergyLevels = applyFlashes flashCoords energyLevels
             newFlashes = Set.union alreadyFlashed (Set.fromList flashCoords)
    in let (finalEnergyLevel, allFlashes) = applyFlashIncreases newFlashes newEnergyLevels
    in (finalEnergyLevel, allFlashes)

getFlashPoints :: Set (Int, Int) -> [[Int]] -> [(Int, Int)]
getFlashPoints alreadyFlashed energyLevels =
    let maxRow = (length energyLevels) - 1
        maxCol = (length (head energyLevels)) - 1
    in let allCoords = [(row, col) | row <- [0..maxRow], col <- [0..maxCol]]
    in filter (shouldFlash alreadyFlashed energyLevels) allCoords

shouldFlash :: Set (Int, Int) -> [[Int]] -> (Int, Int) -> Bool
shouldFlash alreadyFlashed energyLevels (row, col) =
    (((energyLevels !! row) !! col) > 9) && not (Set.member (row, col) alreadyFlashed)

applyFlashes :: [(Int, Int)] -> [[Int]] -> [[Int]]
applyFlashes flashCoords energyLevels =
    let energyDeltas = getEnergyDeltas energyLevels flashCoords
    in applyEnergyDeltas energyDeltas energyLevels

getEnergyDeltas :: [[Int]] -> [(Int, Int)] -> Map (Int, Int) Int
getEnergyDeltas energyLevels flashPoints =
    let increasePoints = concat $ map (getNeighbourCoords energyLevels) flashPoints
    in combineEnergyIncreases Map.empty increasePoints

combineEnergyIncreases :: Map (Int, Int) Int -> [(Int, Int)] -> Map (Int, Int) Int
combineEnergyIncreases deltas [] = deltas
combineEnergyIncreases deltas (p:pts) =
    let existing = Map.findWithDefault 0 p deltas
    in let newDeltas = Map.insert p (existing + 1) deltas
    in combineEnergyIncreases newDeltas pts

getNeighbourCoords :: [[Int]] -> (Int, Int) -> [(Int, Int)]
getNeighbourCoords energyLevels point =
    filter (isValidPoint energyLevels) $ allPossibleNeighbours point

allPossibleNeighbours :: (Int, Int) -> [(Int, Int)]
allPossibleNeighbours (row, col) =
    [ (row + 1, col + 1)
    , (row + 1, col)
    , (row + 1, col - 1)
    , (row, col + 1)
    , (row, col - 1)
    , (row - 1, col + 1)
    , (row - 1, col)
    , (row - 1, col - 1)]

isValidPoint :: [[Int]] -> (Int, Int) -> Bool
isValidPoint energyLevels (row, col) =
    let rowLimit = length energyLevels
        colLimit = length (head energyLevels)
    in row >= 0 && row < rowLimit && col >= 0 && col < colLimit

applyEnergyDeltas :: Map (Int, Int) Int -> [[Int]] -> [[Int]]
applyEnergyDeltas deltas energyLevels =
    let maxRow = (length energyLevels) - 1
        maxCol = (length (head energyLevels)) - 1
    in let coords = [[(row, col) | col <- [0..maxCol]] | row <- [0..maxRow]]
    in map (map (getNewEnergy deltas energyLevels)) coords

getNewEnergy :: Map (Int, Int) Int -> [[Int]] -> (Int, Int) -> Int
getNewEnergy deltas energyLevels (row, col) =
    let delta = Map.findWithDefault 0 (row, col) deltas
        currEnergy = (energyLevels !! row) !! col
    in currEnergy + delta

resetFlashed :: [[Int]] -> Set (Int, Int) -> [[Int]]
resetFlashed energyLevels flashCoords =
    let maxRow = (length energyLevels) - 1
        maxCol = (length (head energyLevels)) - 1
    in let coords = [[(row, col) | col <- [0..maxCol]] | row <- [0..maxRow]]
    in map (map (getResetValue energyLevels flashCoords)) coords

getResetValue :: [[Int]] -> Set (Int, Int) -> (Int, Int) -> Int
getResetValue energyLevels flashCoords (row, col) =
    if Set.member (row, col) flashCoords then 0
    else (energyLevels !! row) !! col

