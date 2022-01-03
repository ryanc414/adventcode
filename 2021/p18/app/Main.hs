module Main where

import System.Environment
import Data.Maybe

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let nums = parseNumbers contents
    let sum = getFinalSum nums
    putStrLn $ showNumber sum
    let magnitude = getMagnitude sum
    print magnitude
    let maxPairMagnitude = getMaxPairMagnitude nums
    print maxPairMagnitude

data SnailfishNumber = RegularNumber Int | PairNumber (SnailfishNumber, SnailfishNumber) deriving (Show, Eq)

parseNumbers :: String -> [SnailfishNumber]
parseNumbers = map parseNumber . lines

parseNumber :: String -> SnailfishNumber
parseNumber contents =
    if (head contents) == '['
    then
        let inner = (tail . init) contents
        in let i = findCentre inner
        in let left = take i inner
               right = drop (i + 1) inner
        in PairNumber (parseNumber left, parseNumber right)
    else
        RegularNumber $ read contents

findCentre :: String -> Int
findCentre = findCentreComma 0
    where
        findCentreComma :: Int -> String -> Int
        findCentreComma level (x:xs) =
            if level == 0 && x == ','
            then 0
            else let
                newLevel = if x == '['
                then level + 1
                else if x == ']'
                then level - 1
                else level
            in
            1 + findCentreComma newLevel xs

showNumber :: SnailfishNumber -> String
showNumber (RegularNumber n) = show n
showNumber (PairNumber (a, b)) = "[" ++ (showNumber a) ++ "," ++ (showNumber b) ++ "]"

getFinalSum :: [SnailfishNumber] -> SnailfishNumber
getFinalSum = foldl1 addNumbers

addNumbers :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
addNumbers a b = reduceNumber $ PairNumber (a, b)

reduceNumber :: SnailfishNumber -> SnailfishNumber
reduceNumber n =
    let (newN, continue) = reduce n
    in if continue then reduceNumber newN
    else newN

reduce :: SnailfishNumber -> (SnailfishNumber, Bool)
reduce n =
    let (newN, exploded) = explodeNumber n
    in if exploded
    then (newN, True)
    else splitNumber n

explodeNumber :: SnailfishNumber -> (SnailfishNumber, Bool)
explodeNumber n =
    let (newN, action) = explode 0 n
    in (newN, isJust action)

explode :: Int -> SnailfishNumber -> (SnailfishNumber, Maybe (Maybe Int, Maybe Int))
explode _ (RegularNumber n) = (RegularNumber n, Nothing)
explode level (PairNumber (a, b)) =
    if level == 4
    then
        let (RegularNumber aa, RegularNumber bb) = (a, b)
        in (RegularNumber 0, Just (Just aa, Just bb))
    else
        let (newA, action) = explode (level + 1) a
        in case action
        of Just (l, r) ->
            case r
            of Just rr ->
                   let newB = addToLeftmost rr b
                   in (PairNumber (newA, newB), Just (l, Nothing))
               Nothing -> (PairNumber (newA, b), action)
           Nothing ->
               let (newB, action) = explode (level + 1) b
               in case action
               of Just (l, r) ->
                   case l
                   of Just ll ->
                          let newA = addToRightmost ll a
                          in (PairNumber (newA, newB), Just (Nothing, r))
                      Nothing -> (PairNumber (a, newB), action)
                  Nothing -> (PairNumber (a, b), Nothing)

addToLeftmost :: Int -> SnailfishNumber -> SnailfishNumber
addToLeftmost x (RegularNumber n) = RegularNumber (x + n)
addToLeftmost x (PairNumber (a, b)) =
    let newA = addToLeftmost x a
    in PairNumber (newA, b)

addToRightmost :: Int -> SnailfishNumber -> SnailfishNumber
addToRightmost x (RegularNumber n) = RegularNumber (x + n)
addToRightmost x (PairNumber (a, b)) =
    let newB = addToRightmost x b
    in PairNumber (a, newB)

splitNumber :: SnailfishNumber -> (SnailfishNumber, Bool)
splitNumber (PairNumber (a, b)) =
    let (newA, split) = splitNumber a
    in if split
    then (PairNumber (newA, b), True)
    else let (newB, split2) = splitNumber b
    in (PairNumber (a, newB), split2)

splitNumber (RegularNumber n) =
    if n >= 10
    then (PairNumber (RegularNumber (n `div` 2), RegularNumber ((n + 1) `div` 2)), True)
    else (RegularNumber n, False)

getMagnitude :: SnailfishNumber -> Int
getMagnitude (RegularNumber n) = n
getMagnitude (PairNumber (a, b)) = (3 * (getMagnitude a)) + (2 * (getMagnitude b))

getMaxPairMagnitude :: [SnailfishNumber] -> Int
getMaxPairMagnitude nums = (maximum . map (getMagnitude . addNumbersPair)) [(a, b) | a <- nums, b <- nums, a /= b]

addNumbersPair :: (SnailfishNumber, SnailfishNumber) -> SnailfishNumber
addNumbersPair (a, b) = addNumbers a b

