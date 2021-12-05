import System.Environment

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let report = parseInput contents
    let power = getPowerConsumption report
    print power
    let lifeSupportRating = getLifeSupportRating report
    print lifeSupportRating

-- Parse string input into a list of binary lists.
parseInput :: String -> [[Bool]]
parseInput = (map parseLine) . lines

-- Parse a string of 0s and 1s into a binary list.
parseLine :: String -> [Bool]
parseLine "" = []
parseLine ('0':xs) = [False] ++ parseLine xs
parseLine ('1':xs) = [True] ++ parseLine xs

-- Get power consumption by calculating gamma and epsilon values and
-- multiplying them together.
getPowerConsumption :: [[Bool]] -> Int
getPowerConsumption report =
    let len = length (head report)
    in let gamma = calculateGamma len report
    in let epsilon = calculateEpsilon gamma
    in (toInt gamma) * (toInt epsilon)

-- Calculate the bits of gamma by finding the mode bits in each position.
calculateGamma :: Int -> [[Bool]] -> [Bool]
calculateGamma n report = go n 0 report
    where
        go :: Int -> Int -> [[Bool]] -> [Bool]
        go n i _ | n == i = []
        go n i report = let mode = getMode i report
            in [mode] ++ go n (i + 1) report

-- Get the mode bit in a particular position.
getMode :: Int -> [[Bool]] -> Bool
getMode i report = let (zeros, ones) = foldr (countDigit i) (0, 0) report
    in ones >= zeros

-- Increments either the zeros or ones count.
countDigit :: Int -> [Bool] -> (Int, Int) -> (Int, Int)
countDigit i val (zeros, ones) = if (val !! i)
    then (zeros, ones + 1) else (zeros + 1, ones)

-- Calculate epsilon from the bitwise not of gamma.
calculateEpsilon :: [Bool] -> [Bool]
calculateEpsilon [] = []
calculateEpsilon (x:xs) = [not x] ++ calculateEpsilon xs

-- Convert a binary list to an integer.
toInt :: [Bool] -> Int
toInt [] = 0
toInt (x:xs) = let rest = (toInt xs)
    in if x then (2 ^ (length xs)) + rest else rest

-- Calculate the life support rating by multiplying the oxygen rating by the
-- CO2 rating.
getLifeSupportRating :: [[Bool]] -> Int
getLifeSupportRating report =
    let oxygenRating = getOxygenRating report
        co2Rating = getCO2Rating report
    in (toInt oxygenRating) * (toInt co2Rating)

-- Find the oxygen rating.
getOxygenRating :: [[Bool]] -> [Bool]
getOxygenRating = filterBitCondition True 0

-- Find the CO2 rating.
getCO2Rating :: [[Bool]] -> [Bool]
getCO2Rating = filterBitCondition False 0

-- Filter for the value that matches the bit conditions.
filterBitCondition :: Bool -> Int -> [[Bool]] -> [Bool]
filterBitCondition _ _ [val] = val
filterBitCondition want i report = let mode = getMode i report
    in let filteredReport = filter (bitCondition i (want == mode)) report
    in filterBitCondition want (i + 1) filteredReport

-- Checks if the i'th binary digit matches what we are filtering for.
bitCondition :: Int -> Bool -> [Bool] -> Bool
bitCondition i want val = (val !! i) == want

