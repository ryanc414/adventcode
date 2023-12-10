import System.Environment
import Data.Char
import Data.List
import Data.Maybe

-- Get filename from commandline, read its contents and print the results for
-- both parts of the problem.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let result1 = sumCalibrationValues1 contents
    print result1
    let result2 = sumCalibrationValues2 contents
    print result2

sumCalibrationValues1 :: String -> Int
sumCalibrationValues1 = sum . map getLineCalibrationVal1 . lines

getLineCalibrationVal1 :: String -> Int
getLineCalibrationVal1 line = 10 * (getFirstDigit1 line) + (getLastDigit1 line)

getFirstDigit1 :: String -> Int
getFirstDigit1 (head:tail) = case convertCharToInt head of
    Just x -> x
    Nothing -> getFirstDigit1 tail
getFirstDigit1 [] = error "No digit found in string"

convertCharToInt :: Char -> Maybe Int
convertCharToInt c
    | isDigit c = Just (digitToInt c)
    | otherwise = Nothing

getLastDigit1 :: String -> Int
getLastDigit1 = getFirstDigit1 . reverse

sumCalibrationValues2 :: String -> Int
sumCalibrationValues2 = sum . map getLineCalibrationVal2 . lines

getLineCalibrationVal2 :: String -> Int
getLineCalibrationVal2 line = 10 * (getFirstDigit2 line) + (getSecondDigit2 line)

getFirstDigit2 :: String -> Int
getFirstDigit2 line = case convertTextToDigit line of
    Just x -> x
    Nothing -> case convertCharToInt (head line) of
        Just x -> x
        Nothing -> getFirstDigit2 (tail line)

convertTextToDigit :: String -> Maybe Int
convertTextToDigit line
    | "one" `isPrefixOf` line = Just 1
    | "two" `isPrefixOf` line = Just 2
    | "three" `isPrefixOf` line = Just 3
    | "four" `isPrefixOf` line = Just 4
    | "five" `isPrefixOf` line = Just 5
    | "six" `isPrefixOf` line = Just 6
    | "seven" `isPrefixOf` line = Just 7
    | "eight" `isPrefixOf` line = Just 8
    | "nine" `isPrefixOf` line = Just 9
    | otherwise = Nothing


getSecondDigit2 :: String -> Int
getSecondDigit2 = getSecondDigit2Reversed . reverse

getSecondDigit2Reversed :: String -> Int
getSecondDigit2Reversed line = case convertTextToDigitReversed line of
    Just x -> x
    Nothing -> case convertCharToInt (head line) of
        Just x -> x
        Nothing -> getSecondDigit2Reversed (tail line)

convertTextToDigitReversed :: String -> Maybe Int
convertTextToDigitReversed line
    | "eno" `isPrefixOf` line = Just 1
    | "owt" `isPrefixOf` line = Just 2
    | "eerht" `isPrefixOf` line = Just 3
    | "ruof" `isPrefixOf` line = Just 4
    | "evif" `isPrefixOf` line = Just 5
    | "xis" `isPrefixOf` line = Just 6
    | "neves" `isPrefixOf` line = Just 7
    | "thgie" `isPrefixOf` line = Just 8
    | "enin" `isPrefixOf` line = Just 9
    | otherwise = Nothing
