module Main where

import Data.Maybe
import Data.List
import System.Environment

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let ls = lines contents
    let processed = map processLine ls
    let errorScore = sumTotalErrorScore processed
    print errorScore
    let middleScore = findMiddleScore processed
    print middleScore

data ProcessedLine = Illegal Char | OpenerStack [Char]

processLine :: String -> ProcessedLine
processLine line =
    go [] line
        where
            go :: [Char] -> String -> ProcessedLine
            go stack [] = OpenerStack stack
            go stack (c:cs) =
                if isOpener c then go ([c] ++ stack) cs
                else if null stack then Illegal c
                else if bracketMatches (head stack) c then go (tail stack) cs
                else Illegal c

sumTotalErrorScore :: [ProcessedLine] -> Int
sumTotalErrorScore = foldr (+) 0 . map getErrorScore . getIllegalChars

getErrorScore :: Char -> Int
getErrorScore ')' = 3
getErrorScore ']' = 57
getErrorScore '}' = 1197
getErrorScore '>' = 25137

getIllegalChars :: [ProcessedLine] -> [Char]
getIllegalChars = catMaybes . map getIllegalChar

getIllegalChar :: ProcessedLine -> Maybe Char
getIllegalChar (Illegal c) = Just c
getIllegalChar (OpenerStack _) = Nothing

isOpener :: Char -> Bool
isOpener '(' = True
isOpener '[' = True
isOpener '{' = True
isOpener '<' = True
isOpener ')' = False
isOpener ']' = False
isOpener '}' = False
isOpener '>' = False

bracketMatches :: Char -> Char -> Bool
bracketMatches a b = getMatchingCloser a == b

findMiddleScore :: [ProcessedLine] -> Int
findMiddleScore = getMedian . map getScore . getCompletionStrings

getCompletionStrings :: [ProcessedLine] -> [String]
getCompletionStrings = catMaybes . map getCompletionString

getCompletionString :: ProcessedLine -> Maybe String
getCompletionString (Illegal _) = Nothing
getCompletionString (OpenerStack s) = Just (map getMatchingCloser s)

getMatchingCloser :: Char -> Char
getMatchingCloser '(' = ')'
getMatchingCloser '[' = ']'
getMatchingCloser '{' = '}'
getMatchingCloser '<' = '>'

getScore :: String -> Int
getScore = foldl (\score c -> (score * 5) + (getCharScore c)) 0

getCharScore :: Char -> Int
getCharScore ')' = 1
getCharScore ']' = 2
getCharScore '}' = 3
getCharScore '>' = 4

getMedian :: [Int] -> Int
getMedian vals =
    let sorted = sort vals
        i = (length vals) `div` 2
    in  sorted !! i

