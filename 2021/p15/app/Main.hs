module Main where

import System.Environment
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Heap (MinHeap)
import qualified Data.Heap as Heap
import Data.Maybe

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let riskMap = parseInput contents
    let risk = findLowestRisk riskMap
    print risk
    let extendedMap = extendMap riskMap
    let risk2 = findLowestRisk extendedMap
    print risk2

showMap :: [[Int]] -> String
showMap grid = intercalate "\n" (map (map intToDigit) grid)

parseInput = map (map digitToInt) . lines

data Node = Node { coords :: (Int, Int), value :: Int, tentativeDistance :: Maybe Int, visited :: Bool } deriving Show

instance Eq Node where
    (Node {coords=c1}) == (Node {coords=c2}) = c1 == c2

instance Ord Node where
    (Node {tentativeDistance=(Just d1)}) `compare` (Node {tentativeDistance=(Just d2)}) = d1 `compare` d2
    (Node {tentativeDistance=(Just _)}) `compare` (Node {tentativeDistance=(Nothing)}) = LT
    (Node {tentativeDistance=(Nothing)}) `compare` (Node {tentativeDistance=(Just _)}) = GT
    (Node {tentativeDistance=(Nothing)}) `compare` (Node {tentativeDistance=(Nothing)}) = EQ

findLowestRisk :: [[Int]] -> Int
findLowestRisk grid =
    let gridMap = listsToMap grid
    in let current = gridMap Map.! (0, 0)
    in explore (length grid) gridMap Heap.empty current

explore :: Int -> Map (Int, Int) Node -> MinHeap Node -> Node -> Int
explore size gridMap unvisited curr =
    let neighbours = getAllNeighbours gridMap curr
        (Just currDist) = tentativeDistance curr
    in let distances = map ((currDist +) . value) neighbours
    in let (newGridMap, newUnvisited) = foldr insertNewDistance (gridMap, unvisited) (zip neighbours distances)
    in if (coords curr) == (size-1, size-1) then
        let (Just result) = (tentativeDistance curr)
        in result
    else
        let newGridMap2 = markVisited curr newGridMap
        in let (next, rest) = getNextUnvisited newUnvisited newGridMap2
        in explore size newGridMap2 rest next

getAllNeighbours :: Map (Int, Int) Node -> Node -> [Node]
getAllNeighbours nodes curr =
    let (row, col) = coords curr
    in let allPossibleNeighbours = [(row + 1, col), (row, col + 1), (row - 1, col), (row, col - 1)]
    in catMaybes $ map (\p -> Map.lookup p nodes) allPossibleNeighbours

insertNewDistance :: (Node, Int) -> (Map (Int, Int) Node, MinHeap Node) -> (Map (Int, Int) Node, MinHeap Node)
insertNewDistance (node, dist) (nodes, unvisited) =
    let cds = coords node
    in let (newDist, update) = case tentativeDistance (nodes Map.! cds) of Just currDist -> if dist < currDist then (dist, True) else (dist, False)
                                                                           Nothing -> (dist, True)
    in let newNode = Node {coords=cds, value=(value node), tentativeDistance=(Just newDist), visited=(visited node)}
    in if update then (Map.insert cds newNode nodes, Heap.insert newNode unvisited)
    else (nodes, unvisited)

markVisited :: Node -> Map (Int, Int) Node -> Map (Int, Int) Node
markVisited node nodes =
    let newNode = Node { coords=(coords node), value=(value node), tentativeDistance=(tentativeDistance node), visited=True}
    in Map.insert (coords node) newNode nodes

getNextUnvisited :: MinHeap Node -> Map (Int, Int) Node -> (Node, MinHeap Node)
getNextUnvisited heap nodes =
    let (next, rest) = case Heap.view heap of Just (n, r) -> (n, r)
                                              Nothing -> error $ show nodes
    in let node = nodes Map.! (coords next)
    in if (visited node) then getNextUnvisited rest nodes
    else (next, rest)

listsToMap :: [[Int]] -> Map (Int, Int) Node
listsToMap grid =
    let maxRowCol = (length grid) - 1
    in let allPoints = [(row, col) | row <- [0..maxRowCol], col <- [0..maxRowCol]]
    in Map.fromList $ map ((\n -> (coords n, n)) . (initNode grid)) allPoints

initNode :: [[Int]] -> (Int, Int) -> Node
initNode grid (row, col) =
    let val = (grid !! row) !! col
        dist = if row == 0 && col == 0 then Just 0 else Nothing
    in Node { coords=(row, col), value=val, tentativeDistance=dist, visited=False }

isValidPoint :: [[Int]] -> (Int, Int) -> Bool
isValidPoint grid (row, col) =
    let maxRowCol = (length grid) - 1
    in row >= 0 && row <= maxRowCol && col >= 0 && col <= maxRowCol

extendMap :: [[Int]] -> [[Int]]
extendMap = extendVertically . extendHorizontally

extendHorizontally :: [[Int]] -> [[Int]]
extendHorizontally = map (\row -> concat (map (\i -> map (\x -> incrementAndWrap i x) row) [0..4]))

extendVertically :: [[Int]] -> [[Int]]
extendVertically grid = concat $ map (\i -> map (map (\x -> incrementAndWrap i x)) grid)  [0..4]

incrementAndWrap :: Int -> Int -> Int
incrementAndWrap i x =
    if i == 0 then x
    else incrementAndWrap (i - 1) ((x `mod` 9) + 1)

