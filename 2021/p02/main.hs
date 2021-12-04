import System.Environment
import Data.List
import Data.Char

-- Get filename from commandline, read its contents and print the results
-- for both parts.
main = do
    [filename] <- getArgs
    contents <- readFile filename
    let instructions = parseInstructions contents
    let result = getPositionMultiple instructions
    print result
    let result2 = getPositionMultiple2 instructions
    print result2

-- Instruction represents a command to move forward, down or up.
data Instruction = Forward Int | Down Int | Up Int

-- Process all instructions and returns the multiple of horizontal position
-- and depth coordinates.
getPositionMultiple :: [Instruction] -> Int
getPositionMultiple instructions =
    let (x, y) = processInstructions instructions
    in x * y

-- Parse all instructions from string input.
parseInstructions :: String -> [Instruction]
parseInstructions = (map parseInstruction) . lines

-- Parse a single instruction.
parseInstruction :: String -> Instruction
parseInstruction line | "forward" `isPrefixOf` line = Forward $ digitToInt (line !! 8)
parseInstruction line | "down" `isPrefixOf` line = Down $ digitToInt (line !! 5)
parseInstruction line | "up" `isPrefixOf` line = Up $ digitToInt (line !! 3)

-- Process all instructions and return the final horizontal position and depth
-- coordinates.
processInstructions :: [Instruction] -> (Int, Int)
processInstructions ins = go (0, 0) ins
    where
        go :: (Int, Int) -> [Instruction] -> (Int, Int)
        go (x, y) [] = (x, y)
        go (x, y) (i:ins) = let (xx, yy) = processInstruction (x, y) i
            in go (xx, yy) ins

-- Process a single instruction and return the new horizontal position and
-- depth coordinates.
processInstruction :: (Int, Int) -> Instruction -> (Int, Int)
processInstruction (x, y) (Forward n) = (x + n, y)
processInstruction (x, y) (Down n) = (x, y + n)
processInstruction (x, y) (Up n) = (x, y - n)

-- Process all instructions according to part 2 rules and returns the multiple
-- of the final horizontal position and depth coordinates.
getPositionMultiple2 :: [Instruction] -> Int
getPositionMultiple2 instructions =
    let (x, y, _) = processInstructions2 instructions
    in x * y

-- Process all instructions according to part 2 rules and returns the final
-- horizontal position, depth and aim coordinates.
processInstructions2 :: [Instruction] -> (Int, Int, Int)
processInstructions2 ins = go (0, 0, 0) ins
    where
        go :: (Int, Int, Int) -> [Instruction] -> (Int, Int, Int)
        go (x, y, z) [] = (x, y, z)
        go (x, y, z) (i:ins) = let (xx, yy, zz) = processInstruction2 (x, y, z) i
            in go (xx, yy, zz) ins

-- Process a single instruction according to part 2 rulwes and returns the
-- updated horizontal position, depth and aim coordinates.
processInstruction2 :: (Int, Int, Int) -> Instruction -> (Int, Int, Int)
processInstruction2 (x, y, z) (Forward n) = (x + n, y + (z * n), z)
processInstruction2 (x, y, z) (Down n) = (x, y, z + n)
processInstruction2 (x, y, z) (Up n) = (x, y, z - n)

