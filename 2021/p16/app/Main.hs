module Main where

import System.Environment

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let bits = parseHexBits contents
    let versionSum = sumVersionNums bits
    print versionSum

parseHexBits :: String -> [Bool]
parseHexBits = concat . map parseHexChar

parseHexChar :: Char -> [Bool]
parseHexChar '0' = parseBits "0000"
parseHexChar '1' = parseBits "0001"
parseHexChar '2' = parseBits "0010"
parseHexChar '3' = parseBits "0011"
parseHexChar '4' = parseBits "0100"
parseHexChar '5' = parseBits "0101"
parseHexChar '6' = parseBits "0110"
parseHexChar '7' = parseBits "0111"
parseHexChar '8' = parseBits "1000"
parseHexChar '9' = parseBits "1001"
parseHexChar 'A' = parseBits "1010"
parseHexChar 'B' = parseBits "1011"
parseHexChar 'C' = parseBits "1100"
parseHexChar 'D' = parseBits "1101"
parseHexChar 'E' = parseBits "1110"
parseHexChar 'F' = parseBits "1111"
parseHexChar '\n' = []

parseBits :: String -> [Bool]
parseBits = map parseBit

parseBit :: Char -> Bool
parseBit '0' = False
parseBit '1' = True

sumVersionNums :: [Bool] -> Int
sumVersionNums bits =
    let (version, _) = parsePacket bits
    in version

parsePacket :: [Bool] -> (Int, [Bool])
parsePacket bits =
    let (versionBits, rest) = splitAt 3 bits
    in let (typeBits, rest2) = splitAt 3 rest
    in let version = bitsToInt versionBits
    in let (restVers, remainderBits) = if (bitsToInt typeBits) == 4
           then (0, parseLiteralPkt rest2)
           else parseOperatorPkt rest2
    in (version + restVers, remainderBits)

bitsToInt :: [Bool] -> Int
bitsToInt [] = 0
bitsToInt (x:xs) = let rest = (bitsToInt xs)
    in if x then (2 ^ (length xs)) + rest else rest

parseLiteralPkt :: [Bool] -> [Bool]
parseLiteralPkt (x:xs) =
    if x then parseLiteralPkt (drop 4 xs) else (drop 4 xs)

parseOperatorPkt :: [Bool] -> (Int, [Bool])
parseOperatorPkt (x:xs) =
    if x then parseOpPkt1 xs else parseOpPkt0 xs

parseOpPkt0 :: [Bool] -> (Int, [Bool])
parseOpPkt0 bits =
    let (lengthBits, rest) = splitAt 15 bits
    in parseSubPktsByLen (bitsToInt lengthBits) rest

parseSubPktsByLen :: Int -> [Bool]  -> (Int, [Bool])
parseSubPktsByLen 0 bits = (0, bits)
parseSubPktsByLen len bits =
    let (version, rest) = parsePacket bits
    in let newLen = len - ((length bits) - (length rest))
    in let (restVersion, remainderBits) = parseSubPktsByLen newLen rest
    in (version + restVersion, remainderBits)

parseOpPkt1 :: [Bool] -> (Int, [Bool])
parseOpPkt1 bits =
    let (numBits, rest) = splitAt 11 bits
    in parseSubPktsByNum (bitsToInt numBits) rest

parseSubPktsByNum :: Int -> [Bool] -> (Int, [Bool])
parseSubPktsByNum 0 bits = (0, bits)
parseSubPktsByNum n bits =
    let (version, rest) = parsePacket bits
    in let (restVersion, remainderBits) = parseSubPktsByNum (n - 1) rest
    in (version + restVersion, remainderBits)

