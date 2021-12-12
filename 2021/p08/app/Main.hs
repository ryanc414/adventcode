module Main where

import System.Environment
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let displayInput = parseInput contents
    let count = countAllUniqueDigits displayInput
    print count

data DisplayInput = DisplayInput {signals :: [Set Digit], outputs :: [Set Digit]} deriving Show

data Digit = A | B | C | D | E | F | G deriving (Eq, Ord, Show, Read, Bounded, Enum)

parseInput :: String -> [DisplayInput]
parseInput = (map parseInputLine) . lines

parseInputLine :: String -> DisplayInput
parseInputLine str =
    let sides = splitOn " | " str
    in let [sigs, outs] = map parseValues sides
    in DisplayInput {signals=sigs, outputs=outs}

parseValues :: String -> [Set Digit]
parseValues = (map parseDigits) . words

parseDigits :: String -> Set Digit
parseDigits = Set.fromList . (map parseDigit)

parseDigit :: Char -> Digit
parseDigit = read . (:[]) . toUpper

countAllUniqueDigits :: [DisplayInput] -> Int
countAllUniqueDigits = (foldr (+) 0) . (map countUnique)

countUnique :: DisplayInput -> Int
countUnique (DisplayInput {outputs = out}) = length $ filter isUnique out

isUnique :: Set Digit -> Bool
isUnique digits = case (length digits) of
    2 -> True
    3 -> True
    4 -> True
    7 -> True
    _ -> False

