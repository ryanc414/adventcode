import System.Environment

-- Get filename from commandline, read its contents and print the results for
-- both parts of the problem.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let result = countInputIncreases contents
    print result
    let result2 = countWindowIncreases contents
    print result2

-- From the raw string input, count the number of times a value increased
-- over the previous one.
countInputIncreases :: String -> Int
countInputIncreases contents = let (x:xs) = parseLines contents in
    countIncreases x xs

-- Sum the values over a moving 3-value window and then count the number of
-- times a window value increased over the previous one.
countWindowIncreases :: String -> Int
countWindowIncreases contents = let (x:xs) = (getWindows . parseLines) contents in
    countIncreases x xs

-- Parse the input string as a list of ints.
parseLines :: String -> [Int]
parseLines = (map read) . lines

-- Count the number of times the next value in a list increased over the
-- previous value.
countIncreases :: Int -> [Int] -> Int
countIncreases _ [] = 0
countIncreases n (x:xs) = let rest = countIncreases x xs in
    if x > n then 1 + rest else rest

-- Convert a list of values to a list of window sums.
getWindows :: [Int] -> [Int]
getWindows xs = if length xs < 3 then
    [] else
    let windowSum = sumNList 3 xs in
    [windowSum] ++ getWindows (tail xs)

-- Sum the first N values of a list, throws exception if list is not long
-- enough.
sumNList :: Int -> [Int] -> Int
sumNList n xs = go 0 xs
    where
        go counter _ | counter == n = 0
        go counter (x:xs) = x + go (counter + 1) xs

