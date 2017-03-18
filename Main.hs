-- Jose Perez, Tomas Chagoya

import Data.List
import Data.Maybe 
import System.IO 
import System.Random
import Board
 
-- Main function to play a Connect Four, will give the
-- player the option to play against the computer or
-- against another player. The board is 7 columns by
-- 6 rows. Input for the choice of game is taken from
-- the pickGame function (found below)
main = do
    let bd = mkBoard 7 6
    let p = mkPlayer
    putStrLn "\nCONNECT FOUR (AGAIN)\n"
    choice <- pickGame
    putStrLn (boardToStr bd playerToChar)
    putStrLn ""

    if(choice == 1)
    then do strategyGame bd p False
    else do game bd p

-- Standard game playing against another player. Will keep
-- Going until either player wins or enters -1 to quit
game :: [[Int]] -> Int -> IO()
game bd p = do
    -- read input from user for column
    col <- readSlot bd p
    -- if it's not the value to quit
    if not(col == (-1))
    then do
        -- prepare an updated board with the token dropped in for next call
        let updatedBoard = dropInSlot bd col p
        -- prepare a board to display
        let drawnBoard = (boardToStr updatedBoard playerToChar)
        -- if the game is won by the current player on the updated board
        if(isWonBy updatedBoard p)
        then do 
            putStrLn drawnBoard
            putStr "Player "
            printWinner p
            putStrLn " won!"
        -- otherwise keep the game going
        else do
            putStrLn drawnBoard
            putStrLn ""
            game updatedBoard (changePlayer p)
    -- if the player entered the exit value (-1)
    else do 
        putStrLn "\nGood bye!\n"

-- Strategy game against a computer. Will keep going until
-- either the computer or the player wins, or the player enters
-- the end game value (-1) to quit
strategyGame :: [[Int]] -> Int -> Bool -> IO()
strategyGame bd p compTurn = do
    -- If it is the computers turn
    if compTurn
    -- get a random value between 0 and 6 and drop it
    -- in the board
    then do
        col <- getRandomSlot
        let updatedBoard = dropInSlot bd col p
        let drawnBoard = (boardToStr updatedBoard playerToChar)
        -- Check if computer won
        if(isWonBy updatedBoard p)
        then do 
            putStrLn drawnBoard
            putStr "Player "
            printWinner p
            putStrLn " won!"
        -- If computer did NOT win, change to player's turn
        else do
            putStrLn drawnBoard
            putStrLn ""
            strategyGame updatedBoard (changePlayer p) (changeTurn compTurn)
    -- If it's the player's turn
    else do
        -- Read input from player's choice for column
        col <- readSlot bd p
        -- if input is not exit value to end game (-1) then proceed
        if not(col == (-1))
        then do
            -- prepare an updated board with the token dropped in for next call
            let updatedBoard = dropInSlot bd col p
            -- prepare an updated board with the token dropped in for next call
            let drawnBoard = (boardToStr updatedBoard playerToChar)
            -- if player won 
            if(isWonBy updatedBoard p)
            then do 
                putStrLn drawnBoard
                putStr "Player "
                printWinner p
                putStrLn " won!"
            -- if player didn't win change to computer's turn
            else do
                putStrLn drawnBoard
                putStrLn ""
                strategyGame updatedBoard (changePlayer p) (changeTurn compTurn)
        -- if player entered the exit value (-1)
        else do 
            putStrLn "\nGood bye!\n"
            
-- gets a random number between 0 and 6 (inclusive)
getRandomSlot :: IO(Int)
getRandomSlot = do
    num <- randomRIO(0,6) :: IO Int
    return num
    
-- Changes the turn from player to computer and vice-versa
changeTurn :: Bool -> Bool
changeTurn current
    | current = False
    | otherwise = True

-- Read a 1-based index of an open slot of a board bd for a player p
-- to drop her disc. The function reads inputs from the standard
-- input (stdin) and returns an IO value such as IO(Int) or
-- IO(Integer).    
readSlot :: [[Int]] -> Int -> IO(Int)        
readSlot bd p = do
    putStrLn ("Player " ++ playerToChar p ++ "'s turn")
    putStr ("Select 0-6 or enter -1 to end: ")
    line <- getLine
    let input = reads line :: [(Int, String)] in
      if length input == 0
      then readSlot'
      else let (col, _) = head input in
        if (col <= numSlot bd) && 
           (col >= 0 && col <= 6 ) && 
           (isSlotOpen bd col)
        then return col
        else do
            if (col == (-1))
            then end
            else do readSlot'
    where
      readSlot' = do
        putStrLn "Invalid index"
        readSlot bd p
      end = do
        return (-1)
        
-- Prompts for either a 1 or a 2 from user
-- Used in main to select normal or computer game
pickGame :: IO(Int)
pickGame = do
    putStrLn "Play vs Computer?"
    putStrLn "1 = Yes"
    putStrLn "2 = No"
    line <- getLine
    let input = reads line :: [(Int, String)] in
      if length input == 0
      then pickGame'
      else let (choice, _) = head input in
        if choice == 1 || choice == 2
        then return choice
        else do
            pickGame'
    where
      pickGame' = do
        putStrLn "Please enter 1 or 2"
        pickGame

-- prints the player as winner
printWinner :: Int -> IO()     
printWinner player = do
    if player == 1
    then putStr "1 (O)"
    else putStr "2 (X)"
        
-- Return a character representation of a player p. It returns a
-- Char value. This function is used to print the current state of a
-- board (see the boardToStr function above).
playerToChar :: Int -> String
playerToChar player
 | (player == 1) = "O "
 | (player == 2) = "X "
 | otherwise = ". "