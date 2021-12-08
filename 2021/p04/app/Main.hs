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

isWinningBoard :: Board -> Bool
isWinningBoard board = (anyRowMarked board) || (anyColMarked board)

anyRowMarked :: Board -> Bool
anyRowMarked (Board []) = False
anyRowMarked (Board (row:rows)) = if all isMarked row then True else anyRowMarked (Board rows)

isMarked :: BoardNumber -> Bool
isMarked (Marked _) = True
isMarked (Unmarked _) = False


anyColMarked :: Board -> Bool
anyColMarked (Board rows) =
    if any null rows then False else
        let (col,rest) = unzip $ map (\row -> let Just elems = uncons row in elems) rows
        in if all isMarked col then True else anyColMarked (Board rest)


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

