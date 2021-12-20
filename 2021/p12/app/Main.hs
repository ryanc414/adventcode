module Main where

import System.Environment
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.List.Split

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let caves = parseInput contents
    let count = countPaths caves
    print count
    let count2 = countPaths2 caves
    print count2

data Cave = Cave {name :: String, isBig :: Bool, connections :: [String]} deriving (Show, Eq, Ord)

parseInput :: String -> Map String Cave
parseInput = fromConnections . map parseConnection . lines

data Connection = Connection {fromCave :: String, toCave :: String}

parseConnection :: String -> Connection
parseConnection line =
    let [from, to] = splitOn "-" line
    in Connection {fromCave=from, toCave=to}

fromConnections :: [Connection] -> Map String Cave
fromConnections [] = Map.empty
fromConnections (c:cs) =
    let caves = Map.fromList [(fromCave c, newCave (fromCave c) (toCave c))
                             , (toCave c, newCave (toCave c) (fromCave c))]
        rest = fromConnections cs
    in Map.unionWith mergeConnections caves rest

newCave :: String -> String -> Cave
newCave from to = Cave {name=from, isBig=(caveIsBig from), connections=[to]}

caveIsBig :: String -> Bool
caveIsBig = all isUpper

mergeConnections :: Cave -> Cave -> Cave
mergeConnections a b =
    let conns = (connections a) ++ (connections b)
    in Cave{name=(name a), isBig=(isBig a), connections=conns}

countPaths :: Map String Cave -> Int
countPaths caves =
    let start = caves Map.! "start"
    in let paths = explore caves Set.empty start
    in length paths

countPaths2 :: Map String Cave -> Int
countPaths2 caves =
    let start = caves Map.! "start"
    in let paths = explore2 caves Set.empty False start
    in length paths

explore :: Map String Cave -> Set String -> Cave -> [[Cave]]
explore caves visited current =
    if (name current) == "end" then [[current]]
    else let toVisit = map (caves Map.!) $ filter (\conn -> not (Set.member conn visited)) (connections current)
             newVisited = if isBig current then visited else Set.insert (name current) visited
         in concat $ map ((map ([current] ++)) . (explore caves newVisited)) toVisit

explore2 :: Map String Cave -> Set String -> Bool -> Cave -> [[Cave]]
explore2 caves visited revisited current =
    if (name current) == "end" then [[current]]
    else let newRevisited = revisited || Set.member (name current) visited
    in let toVisit = map (caves Map.!) $ filter (canVisitNext visited newRevisited) (connections current)
           newVisited = if isBig current then visited else Set.insert (name current) visited
       in concat $ map ((map ([current] ++)) . (explore2 caves newVisited newRevisited)) toVisit

canVisitNext :: Set String -> Bool -> String -> Bool
canVisitNext visited revisited conn =
    if conn == "start" then False
    else if not revisited then True
    else not $ Set.member conn visited

