import System.Environment

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let initialFish = parseInput contents
    let numFish = countFishAfter 18
    print numFish

parseInput :: String -> [Int]
parseInput str =

