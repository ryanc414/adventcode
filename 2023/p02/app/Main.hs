import System.Environment
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

import Text.Regex.Posix

-- Get filename from commandline, read its contents and print the results for
-- both parts of the problem.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let games = parseGames contents

    let result1 = sumPossibleGameIDs games
    print result1

    let result2 = sumPowerMinSets games
    print result2


data Game = Game { gameID :: Int
                 , sets :: [Set]
                 } deriving (Show)

data Set = Set { red :: Int
               , green :: Int
               , blue :: Int
               } deriving (Show)

parseGames :: String -> [Game]
parseGames = map parseGame . lines

sumPossibleGameIDs :: [Game] -> Int
sumPossibleGameIDs = sum . map gameID . filter possibleGame

parseGame :: String -> Game
parseGame input =
    let (_, _, _, matches) = input =~ "^Game ([0-9]+): (.*)$" :: (String, String, String, [String])
    in case matches of
        [gameID, sets] -> Game (read gameID) (map parseSet $ splitOn ";" sets)
        _ -> error "Invalid input"

parseSet :: String -> Set
parseSet = foldl mergeSets emptySet . map parseSetElement . splitOn ","

parseSetElement :: String -> Set
parseSetElement input =
    let (_, _, _, matches) = input =~ "^ *([0-9]+) *(red|green|blue)$" :: (String, String, String, [String])
    in case matches of
        [num, colour] -> case colour of
            "red" -> Set (read num) 0 0
            "green" -> Set 0 (read num) 0
            "blue" -> Set 0 0 (read num)
        _ -> error "Invalid input"

mergeSets :: Set -> Set -> Set
mergeSets (Set r1 g1 b1) (Set r2 g2 b2) = Set (r1 + r2) (g1 + g2) (b1 + b2)

emptySet = Set 0 0 0

possibleGame :: Game -> Bool
possibleGame = all possibleSet . sets

possibleSet :: Set -> Bool
possibleSet (Set r g b) = r <= 12 && g <= 13 && b <= 14

sumPowerMinSets :: [Game] -> Int
sumPowerMinSets = sum . map powerMinSet

powerMinSet :: Game -> Int
powerMinSet = getPower . getMinSet

getPower :: Set -> Int
getPower (Set r g b) = r * g * b

getMinSet :: Game -> Set
getMinSet = foldl getMinSetPair  emptySet . sets

getMinSetPair :: Set -> Set -> Set
getMinSetPair (Set r0 g0 b0) (Set r1 g1 b1) = Set (max r0 r1) (max g0 g1) (max b0 b1)
