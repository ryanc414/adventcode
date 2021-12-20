module Main where

import System.Environment
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split
--
-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let initialState = parseInput contents
    let numDots = countDotsAfterFold initialState
    print numDots

data PuzzleState = PuzzleState { points :: Set (Int, Int), folds :: [Fold] } deriving Show
data Fold = FoldX Int | FoldY Int deriving Show

parseInput :: String -> PuzzleState
parseInput contents =
    let [pointsInput, foldsInput] = splitOn "\n\n" contents
    in PuzzleState { points=(parsePoints pointsInput), folds=(parseFolds foldsInput)}

parsePoints :: String -> Set (Int, Int)
parsePoints = Set.fromList . map parsePoint . lines

parsePoint :: String -> (Int, Int)
parsePoint line =
    let [x, y] = map read $ splitOn "," line
    in (x, y)

parseFolds :: String -> [Fold]
parseFolds = map parseFold . lines

parseFold :: String -> Fold
parseFold line =
    let val = read $ drop 13 line
    in case line !! 11 of
        'x' -> FoldX val
        'y' -> FoldY val

countDotsAfterFold :: PuzzleState -> Int
countDotsAfterFold = Set.size . points . applyNextFold

applyNextFold :: PuzzleState -> PuzzleState
applyNextFold (PuzzleState {points=pts, folds=((FoldX foldx):restFolds)}) =
    let newPoints = Set.map (\(x, y) -> (foldx - (abs (x - foldx)), y)) pts
    in PuzzleState {points=newPoints, folds=restFolds}
applyNextFold (PuzzleState {points=pts, folds=((FoldY foldy):restFolds)}) =
    let newPoints = Set.map (\(x, y) -> (x, foldy - (abs (y - foldy)))) pts
    in PuzzleState {points=newPoints, folds=restFolds}

