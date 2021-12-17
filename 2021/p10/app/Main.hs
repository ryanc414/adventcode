module Main where

import Data.Maybe
import System.Environment

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let ls = lines contents
    let errorScore = sumTotalErrorScore ls
    print errorScore

sumTotalErrorScore :: [String] -> Int
sumTotalErrorScore = foldr (+) 0 . map getErrorScore . getIllegalChars

getErrorScore :: Char -> Int
getErrorScore ')' = 3
getErrorScore ']' = 57
getErrorScore '}' = 1197
getErrorScore '>' = 25137

getIllegalChars :: [String] -> [Char]
getIllegalChars = catMaybes . map findIllegalChar

findIllegalChar :: String -> Maybe Char
findIllegalChar line =
    go [] line
        where
            go :: [Char] -> String -> Maybe Char
            go _ [] = Nothing
            go stack (c:cs) =
                if isOpener c then go ([c] ++ stack) cs
                else if null stack then Just c
                else if bracketMatches (head stack) c then go (tail stack) cs
                else Just c

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
bracketMatches '(' ')' = True
bracketMatches '[' ']' = True
bracketMatches '{' '}' = True
bracketMatches '<' '>' = True
bracketMatches _ _ = False

