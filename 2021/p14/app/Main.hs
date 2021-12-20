module Main where

import System.Environment
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let initialState = parseInput contents
    let diff = findQuantityDiff 10 initialState
    print diff
    let diff2 = findQuantityDiff 40 initialState
    print diff2

data PolymerState = PolymerState { pairs :: Map (Char, Char) Integer
                                 , chainStart :: Char
                                 , rules :: Map (Char, Char) Char } deriving Show

parseInput :: String -> PolymerState
parseInput contents =
    let [initChain, rawRules] = splitOn "\n\n" contents
    in PolymerState { pairs=(parsePairs initChain)
                    , chainStart=(head initChain)
                    , rules=(parseRules rawRules) }

parsePairs :: [Char] -> Map (Char, Char) Integer
parsePairs [a] = Map.empty
parsePairs (a:b:rest) = Map.unionWith (+) (Map.singleton (a, b) 1) (parsePairs ([b] ++ rest))

parseRules :: String -> Map (Char, Char) Char
parseRules = Map.fromList . map parseRule . lines

parseRule :: String -> ((Char, Char), Char)
parseRule line =
    let [[a, b], [to]] = splitOn " -> " line
    in ((a, b), to)

findQuantityDiff :: Int -> PolymerState -> Integer
findQuantityDiff n = getQuantityDiff . applyPairInsertion n

getQuantityDiff :: PolymerState -> Integer
getQuantityDiff state =
    let quantities = getQuantities state
    in let maxQuantity = foldr1 max quantities
           minQuantity = foldr1 min quantities
    in maxQuantity - minQuantity

getQuantities :: PolymerState -> [Integer]
getQuantities = map (\(_, x) -> x) .Map.toList . getQuantityMaps

getQuantityMaps :: PolymerState -> Map Char Integer
getQuantityMaps (PolymerState { pairs=ps, chainStart=start }) =
    Map.foldrWithKey (\(_, c) count m -> Map.unionWith (+) (Map.singleton c count) m) (Map.singleton start 1) ps

applyPairInsertion :: Int -> PolymerState -> PolymerState
applyPairInsertion 0 = id
applyPairInsertion n = applyPairInsertion (n - 1) . insertPairs

insertPairs :: PolymerState -> PolymerState
insertPairs (PolymerState { pairs=ps, chainStart=start, rules=rls }) =
    let newPs = foldr (Map.unionWith (+)) Map.empty $ map (Map.fromList . stepPair rls) $ Map.toList ps
    in PolymerState { pairs=newPs, chainStart=start, rules=rls }

stepPair :: Map (Char, Char) Char -> ((Char, Char), Integer) -> [((Char, Char), Integer)]
stepPair rls ((a, b), count) =
    case Map.lookup (a, b) rls of Just c -> [((a, c), count), ((c, b), count)]
                                  Nothing -> [((a, b), count)]

