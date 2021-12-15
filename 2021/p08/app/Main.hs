module Main where

import System.Environment
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let displayInput = parseInput contents
    let count = countAllUniqueSegments displayInput
    print count
    let val = sumOutputValues displayInput
    print val

data DisplayInput = DisplayInput {signals :: [Set Segment], outputs :: [Set Segment]} deriving Show

data Segment = A | B | C | D | E | F | G deriving (Eq, Ord, Show, Read, Bounded, Enum)

parseInput :: String -> [DisplayInput]
parseInput = (map parseInputLine) . lines

parseInputLine :: String -> DisplayInput
parseInputLine str =
    let sides = splitOn " | " str
    in let [sigs, outs] = map parseValues sides
    in DisplayInput {signals=sigs, outputs=outs}

parseValues :: String -> [Set Segment]
parseValues = (map parseSegments) . words

parseSegments :: String -> Set Segment
parseSegments = Set.fromList . (map parseSegment)

parseSegment :: Char -> Segment
parseSegment = read . (:[]) . toUpper

countAllUniqueSegments :: [DisplayInput] -> Int
countAllUniqueSegments = (foldr (+) 0) . (map countUnique)

countUnique :: DisplayInput -> Int
countUnique (DisplayInput {outputs = out}) = length $ filter isUnique out

isUnique :: Set Segment -> Bool
isUnique segments= case (length segments) of
    2 -> True
    3 -> True
    4 -> True
    7 -> True
    _ -> False

sumOutputValues :: [DisplayInput] -> Int
sumOutputValues = (foldr (+) 0) . (map getOutputValue)

getOutputValue :: DisplayInput -> Int
getOutputValue DisplayInput{signals=sigs, outputs=out} =
    let connections = determineConnections sigs
    in calculateOutput connections out

determineConnections :: [Set Segment] -> Map Segment Segment
determineConnections sigs = go sigs allArrangements
    where
        go :: [Set Segment] -> [Map Segment Segment] -> Map Segment Segment
        go sigs [] = error $ show ("no valid connections", sigs)
        go sigs (x:xs) = if allValidArrangements x sigs then x else go sigs xs

allValidArrangements :: Map Segment Segment -> [Set Segment] -> Bool
allValidArrangements connections = all (validDigit connections)

allArrangements = map (\p -> Map.fromList (zip [(A)..G] p)) (permutations [(A)..G])

validDigit :: Map Segment Segment -> Set Segment -> Bool
validDigit connections sigs =
    case getDigit connections sigs of
        (Just _) -> True
        Nothing -> False

getDigit :: Map Segment Segment -> Set Segment -> Maybe Int
getDigit connections sigs =
    case Set.map (connections Map.!) sigs of
        d | d == Set.fromList [A, B, C, E, F, G] -> Just 0
        d | d == Set.fromList [C, F] -> Just 1
        d | d == Set.fromList [A, C, D, E, G] -> Just 2
        d | d == Set.fromList [A, C, D, F, G] -> Just 3
        d | d == Set.fromList [B, C, D, F] -> Just 4
        d | d == Set.fromList [A, B, D, F, G] -> Just 5
        d | d == Set.fromList [A, B, D, E, F, G] -> Just 6
        d | d == Set.fromList [A, C, F] -> Just 7
        d | d == Set.fromList [(A)..G] -> Just 8
        d | d == Set.fromList [A, B, C, D, F, G] -> Just 9
        _ -> Nothing

calculateOutput :: Map Segment Segment -> [Set Segment] -> Int
calculateOutput connections = (foldl1 (\a b -> 10 * a + b)) . (map (getSingleOutputSegment connections))

getSingleOutputSegment :: Map Segment Segment -> Set Segment -> Int
getSingleOutputSegment connections segments =
    let (Just x) = getDigit connections segments
    in x

