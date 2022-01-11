module Main where

import System.Environment
import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let state = parseInput contents
    let finalState = incrementStateN 2 state
    print $ countPixels finalState
    let finalState2 = incrementStateN 50 state
    print $ countPixels finalState2

data ImageState = ImageState { algorithm :: (Set Int)
                             , image :: (Set (Int, Int))
                             , background :: Bool
                             , bounds :: ((Int, Int), (Int, Int)) } deriving Show

showState :: ImageState -> String
showState state =
    let ((minRow, minCol), (maxRow, maxCol)) = bounds state
    in let pts = [[(row, col) | col <- [(minCol-1)..(maxCol+1)]] | row <- [(minRow-1)..(maxRow+1)]]
    in intercalate "\n" $ map (map (pixelToChar state)) pts

pixelToChar :: ImageState -> (Int, Int) -> Char
pixelToChar (ImageState {image=img}) pt =
    if Set.member pt img then '#' else '.'

parseInput :: String -> ImageState
parseInput contents =
    let [first, second] = splitOn "\n\n" contents
    in let algo = parseAlgorithm first
           img = parseImage second
    in ImageState { algorithm=algo, image=img, background=False, bounds=(findImageBounds img) }

parseAlgorithm :: String -> Set Int
parseAlgorithm = listToSet . (map parseChar)

parseChar :: Char -> Bool
parseChar '#' = True
parseChar '.' = False
parseChar n = error $ show n

listToSet :: [Bool] -> Set Int
listToSet = go 0
    where
        go :: Int -> [Bool] -> Set Int
        go _ [] = Set.empty
        go n (True:xs) = Set.insert n (go (n + 1) xs)
        go n (False:xs) = go (n + 1) xs

parseImage :: String -> Set (Int, Int)
parseImage = listsToSet  . map (map parseChar)  . lines

listsToSet :: [[Bool]] -> Set (Int, Int)
listsToSet = rowsToSet 0

rowsToSet :: Int -> [[Bool]] -> Set (Int, Int)
rowsToSet _ [] = Set.empty
rowsToSet row (x:xs) = Set.union (rowToSet (row, 0) x) (rowsToSet (row + 1) xs)

rowToSet :: (Int, Int) -> [Bool] -> Set (Int, Int)
rowToSet _ [] = Set.empty
rowToSet (row, col) (True:xs) = Set.insert (row, col) (rowToSet (row, col + 1) xs)
rowToSet (row, col) (False:xs) = rowToSet (row, col + 1) xs

countFinalPixels :: ImageState -> Int
countFinalPixels = countPixels . (incrementStateN 2)

countPixels :: ImageState -> Int
countPixels (ImageState {image=img}) = Set.size img

incrementStateN :: Int -> ImageState -> ImageState
incrementStateN 0 state = state
incrementStateN n state = incrementStateN (n - 1) $ incrementState state

incrementState :: ImageState -> ImageState
incrementState state =
    let ((minRow, minCol), (maxRow, maxCol)) = bounds state
    in let pts = [(row, col) | row <- [(minRow - 1)..(maxRow + 1)], col <- [(minCol - 1)..(maxCol + 1)]]
    in let newImage = Set.fromList $ filter (isPixelOn state) pts
    in ImageState {image=newImage, algorithm=(algorithm state), background=(not (background state)), bounds=(findImageBounds newImage)}

findImageBounds :: Set (Int, Int) -> ((Int, Int), (Int, Int))
findImageBounds img =
    let points = Set.toList img
    in let rows = map (\(row, _) -> row) points
           cols = map (\(_, col) -> col) points
    in ((minimum rows, minimum cols), (maximum rows, maximum cols))

isPixelOn :: ImageState -> (Int, Int) -> Bool
isPixelOn state (row, col) =
    let grid = [(row - 1, col - 1), (row - 1, col), (row - 1, col + 1)
               ,(row, col - 1), (row, col), (row, col + 1)
               ,(row + 1, col - 1), (row + 1, col), (row + 1, col + 1)]
    in let pixels = map (getPixelValue state) grid
    in Set.member (binaryToInt pixels) (algorithm state)

getPixelValue :: ImageState -> (Int, Int) -> Bool
getPixelValue (ImageState {image=img, background=bg, bounds=((minRow, minCol), (maxRow, maxCol))}) (row, col) =
    if row >= minRow && row <= maxRow && col >= minCol && col <= maxCol
    then Set.member (row, col) img
    else bg

binaryToInt :: [Bool] -> Int
binaryToInt [] = 0
binaryToInt (x:xs) = let rest = (binaryToInt xs)
    in if x then (2 ^ (length xs)) + rest else rest

