module Main where

import System.Environment
import Data.List
import Data.List.Split

-- Read and parse input file into a game of bingo, play the game to find the
-- winner, then print the winning board's final score.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let game = parseInput contents
    let winningScore = getWinningScore game
    print winningScore
    let losingScore = getLosingScore game
    print losingScore

data Game = Game { numbers :: [Int], boards :: [Board] } deriving Show

newtype Board = Board [[BoardNumber]] deriving Show

data BoardNumber = Marked Int | Unmarked Int deriving Show

parseInput :: String -> Game
parseInput contents =
    let sections = splitOn "\n\n" contents
    in
        let numbers = parseNumbers (head sections)
            boards = parseBoards (tail sections)
        in  Game numbers boards

parseNumbers :: String -> [Int]
parseNumbers s =
    let strs = splitOn "," s
    in map read strs

parseBoards :: [String] -> [Board]
parseBoards = map parseBoard

parseBoard :: String -> Board
parseBoard = Board . (map ((map (Unmarked . read)) . words)) . lines

getWinningScore :: Game -> Int
getWinningScore game =
    let newGame = incrementGameState game
    in case findWinningBoard (boards newGame) of
        Just board -> calculateBoardScore board (head (numbers game))
        Nothing -> getWinningScore newGame

getLosingScore :: Game -> Int
getLosingScore game =
    let newGame = incrementGameState game
    in let (winningBoards, losingBoards) = splitWinnersLosers (boards newGame)
    in if null losingBoards then
        let [finalWinner] = winningBoards
        in calculateBoardScore finalWinner (head (numbers game))
        else getLosingScore (Game (numbers newGame) losingBoards)

incrementGameState :: Game -> Game
incrementGameState game =
    let (n:ns) = numbers game
    in
        let newBoards = map (markNumberInBoard n) (boards game)
        in Game ns newBoards

markNumberInBoard :: Int -> Board -> Board
markNumberInBoard _ (Board []) = (Board [])
markNumberInBoard n (Board (row:rows)) =
    let newRow = markNumberInRow n row
        (Board restRows) = markNumberInBoard n (Board rows)
    in Board ([newRow] ++ restRows)

markNumberInRow :: Int -> [BoardNumber] -> [BoardNumber]
markNumberInRow n = map (markNumber n)

markNumber :: Int -> BoardNumber -> BoardNumber
markNumber n (Unmarked m) = if n == m then (Marked n) else (Unmarked m)
markNumber _ (Marked n) = Marked n

findWinningBoard :: [Board] -> Maybe Board
findWinningBoard [] = Nothing
findWinningBoard (b:bs) = if isWinningBoard b then Just b else findWinningBoard bs

splitWinnersLosers :: [Board] -> ([Board], [Board])
splitWinnersLosers boards =
    let results = map (\b -> (b, isWinningBoard b)) boards
    in
        let winners = map (\(b, _) -> b) (filter (\(b, won) -> won) results)
            losers = map (\(b, _) -> b) (filter (\(b, won) -> not won) results)
        in (winners, losers)

isWinningBoard :: Board -> Bool
isWinningBoard board = (anyRowMarked board) || (anyColMarked board)

anyRowMarked :: Board -> Bool
anyRowMarked (Board rows) = any (all isMarked) rows

isMarked :: BoardNumber -> Bool
isMarked (Marked _) = True
isMarked (Unmarked _) = False

anyColMarked :: Board -> Bool
anyColMarked (Board rows) =
    let transposed = transpose rows
    in anyRowMarked (Board transposed)

calculateBoardScore :: Board -> Int -> Int
calculateBoardScore board justCalled =
    let unmarkedSum = sumUnmarkedNumbers board
    in  unmarkedSum * justCalled

sumUnmarkedNumbers :: Board -> Int
sumUnmarkedNumbers (Board rows) = foldr (+) 0 (map sumUnmarkedInRow rows)

sumUnmarkedInRow :: [BoardNumber] -> Int
sumUnmarkedInRow row =
    foldr sumNextUnmarked 0 row
    where
        sumNextUnmarked :: BoardNumber -> Int -> Int
        sumNextUnmarked (Unmarked n) m = n + m
        sumNextUnmarked (Marked _) m = m

