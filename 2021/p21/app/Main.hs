module Main where

import System.Environment


-- Get inputs from commandline and print the results for both parts.
main = do
    args <- getArgs
    let [start1, start2] = map read args
    let initialState = initState start1 start2
    let result = multiplyFinalScoreRolls initialState
    print result

data PlayerState = PlayerState { position :: Int, score :: Int } deriving Show
data GameState = GameState { players :: [PlayerState]
                           , lastRoll :: Int
                           , numRolls :: Int
                           , nextPlayer :: Int } deriving Show

initState :: Int -> Int -> GameState
initState start1 start2 =
    GameState { players=[ PlayerState { position=start1, score=0}
                        , PlayerState { position=start2, score=0}]
              , lastRoll=0
              , numRolls=0
              , nextPlayer=0 }

multiplyFinalScoreRolls :: GameState -> Int
multiplyFinalScoreRolls state =
    let finalState = playGame state
    in (losingScore finalState) * (numRolls finalState)

playGame :: GameState -> GameState
playGame state =
    let newState = playRound state
    in if gameIsFinished newState then newState else playGame newState

playRound :: GameState -> GameState
playRound (GameState { players=players, lastRoll=lastRoll, numRolls=numRolls, nextPlayer=nextPlayer }) =
    let player = players !! nextPlayer
        (total, last) = rollDice lastRoll 3
    in let newPlayer = incrementPosition player total
    in let newPlayers = insertNewPlayer newPlayer nextPlayer players
    in GameState { players=newPlayers
                 , lastRoll=last
                 , numRolls=(numRolls + 3)
                 , nextPlayer=((nextPlayer + 1) `mod` (length players) ) }

rollDice :: Int -> Int -> (Int, Int)
rollDice last 0 = (0, last)
rollDice last num =
    let roll = (last `mod` 100) + 1
    in let (total, newLast) = rollDice roll (num - 1)
    in (roll + total, newLast)

incrementPosition :: PlayerState -> Int -> PlayerState
incrementPosition player increment =
    let newPosition = ((((position player) - 1) + increment) `mod` 10) + 1
    in PlayerState { position=newPosition
                   , score=((score player) + newPosition) }

insertNewPlayer :: PlayerState -> Int -> [PlayerState] -> [PlayerState]
insertNewPlayer player 0 (p:ps) = player:ps
insertNewPlayer player i (p:ps) = p : (insertNewPlayer player (i - 1) ps)

gameIsFinished :: GameState -> Bool
gameIsFinished = any (\p -> (score p) >= 1000) . players

losingScore :: GameState -> Int
losingScore = minimum . map score . players

