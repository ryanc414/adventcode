module Main where

import System.Environment

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let bits = parseHexBits contents
    let (pkt, _) = parsePacket bits
    let sum = sumVersions pkt
    print sum
    let val = evaluatePkt pkt
    print val

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

data Packet = Literal { version :: Int, value :: Int } |
              Operator { version :: Int, operator :: Op, subpackets :: [Packet] } deriving Show
data Op = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo deriving (Show,Enum)

sumVersions :: Packet -> Int
sumVersions (Literal { version=vers }) = vers
sumVersions (Operator { version=vers, subpackets=subPkts }) =
    foldr ((+) . sumVersions) vers subPkts

evaluatePkt :: Packet -> Int
evaluatePkt (Literal { value=val }) = val
evaluatePkt (Operator { operator=op, subpackets=subpkts }) =
    let subPktVals = map evaluatePkt subpkts
    in case op
    of Sum -> foldr (+) 0 subPktVals
       Product -> foldr (*) 1 subPktVals
       Minimum -> minimum subPktVals
       Maximum -> maximum subPktVals
       GreaterThan -> let [a, b] = subPktVals
                      in if a > b then 1 else 0
       LessThan -> let [a, b] = subPktVals
                   in if a < b then 1 else 0
       EqualTo -> let [a, b] = subPktVals
                  in if a == b then 1 else 0

parsePacket :: [Bool] -> (Packet, [Bool])
parsePacket bits =
    let (versionBits, rest) = splitAt 3 bits
    in let (typeBits, rest2) = splitAt 3 rest
    in let vers = bitsToInt versionBits
           typeNum = bitsToInt typeBits
    in if typeNum == 4
    then parseLiteralPkt vers rest2
    else parseOperatorPkt rest2 vers typeNum

bitsToInt :: [Bool] -> Int
bitsToInt [] = 0
bitsToInt (x:xs) = let rest = (bitsToInt xs)
    in if x then (2 ^ (length xs)) + rest else rest

parseLiteralPkt :: Int -> [Bool] -> (Packet, [Bool])
parseLiteralPkt vers bits =
    let (literalBits, restBits) = getLiteralBits bits
    in (Literal {version=vers, value=(bitsToInt literalBits)}, restBits)

getLiteralBits :: [Bool] -> ([Bool], [Bool])
getLiteralBits (x:xs) =
    let (nextBits, rest) = splitAt 4 xs
    in let (restLiteral, restBits) = if x then getLiteralBits rest else ([], rest)
    in (nextBits ++ restLiteral, restBits)

parseOperatorPkt :: [Bool] -> Int -> Int -> (Packet, [Bool])
parseOperatorPkt (x:xs) =
    if x then parseOpPkt1 xs else parseOpPkt0 xs

parseOpPkt0 :: [Bool] -> Int -> Int -> (Packet, [Bool])
parseOpPkt0 bits vers typeNum =
    let (lengthBits, rest) = splitAt 15 bits
    in let (subPkts, restBits) = parseSubPktsByLen (bitsToInt lengthBits) rest
    in (Operator { version=vers, operator=(parseOperator typeNum), subpackets=subPkts}, restBits)

parseOperator :: Int -> Op
parseOperator 0 = Sum
parseOperator 1 = Product
parseOperator 2 = Minimum
parseOperator 3 = Maximum
parseOperator 5 = GreaterThan
parseOperator 6 = LessThan
parseOperator 7 = EqualTo
parseOperator n = error $ show n

parseSubPktsByLen :: Int -> [Bool] -> ([Packet], [Bool])
parseSubPktsByLen 0 bits = ([], bits)
parseSubPktsByLen len bits =
    let (pkt, rest) = parsePacket bits
    in let newLen = len - ((length bits) - (length rest))
    in let (restPkts, remainderBits) = parseSubPktsByLen newLen rest
    in ([pkt] ++ restPkts, remainderBits)

parseOpPkt1 :: [Bool] -> Int -> Int -> (Packet, [Bool])
parseOpPkt1 bits vers typeNum =
    let (numBits, rest) = splitAt 11 bits
    in let (subPkts, restBits) = parseSubPktsByNum (bitsToInt numBits) rest
    in (Operator { version=vers, operator=(parseOperator typeNum), subpackets=subPkts }, restBits)

parseSubPktsByNum :: Int -> [Bool] -> ([Packet], [Bool])
parseSubPktsByNum 0 bits = ([], bits)
parseSubPktsByNum n bits =
    let (pkt, rest) = parsePacket bits
    in let (restPkts, remainderBits) = parseSubPktsByNum (n - 1) rest
    in ([pkt] ++ restPkts, remainderBits)

