-- Jose Perez, Tomas Chagoya

module Board(mkBoard, mkPlayer, mkOpponent, mkComputer, dropInSlot,
             isSlotOpen, numSlot, slotHeight, isFull, 
             isWonBy, boardToStr, changePlayer) where

import Data.List
import Data.List.Split
import Data.Array
import Data.Maybe

--  Create a board of size m by n (columns by rows)
--  It's implemented as a List of Lists
--  We do this by creating a List of size (m * n) filled with zeros
--  And then we split it into chunks of n size (n chunks of m elements)
mkBoard :: Int -> Int -> [[Int]]
mkBoard m n = (chunksOf n listOfZeros)
    -- Create a List of size (m * n) filled with zeros
    where listOfZeros = (take (m * n) (repeat 0))

--  The players
mkPlayer :: Int
mkPlayer = 1

mkOpponent :: Int
mkOpponent = 2

mkComputer :: Int
mkComputer = 3

-- Changes to the opposite player
changePlayer :: Int -> Int
changePlayer player
    | player == 1 = mkOpponent
    | otherwise = mkPlayer

--  Drop a player's disc in a slot (column) index of a board bd. 
--  The specified slot is assumed to have an empty place to hold the dropped disc
--  Done by replacing a column of the board with a new column
--      Where the new column has the replaced token
--  Returns a new board
dropInSlot :: [[Int]] -> Int -> Int -> [[Int]]
dropInSlot bd index player = replaceColumn bd index newColumn
    where oldColumn = getColumn bd index;
          -- Find the next available empty spot
          emptySpotIndex = getEmptySpot oldColumn;
          -- Replace it. This creates a new column
          newColumn = replaceAt emptySpotIndex player oldColumn
          
--  Finds the next available empty spot in a specified column
--  Returns the index of that spot
getEmptySpot column = last (findIndices(==0) column)

--  Gets the column of board bd at the specified index
--  Assumes the columnIndex is valid
getColumn :: [[Int]] -> Int -> [Int]   
getColumn bd columnIndex = bd !! columnIndex

--  Replaces the specified column (by index) with a
--  new provided column (List)in board bd
replaceColumn :: [[Int]] -> Int -> [Int] -> [[Int]]
replaceColumn bd oldIndex newColumn = firstPart ++ [newColumn] ++ lastPart
    where firstPart = take (oldIndex) bd;
          lastPart = drop (oldIndex + 1) bd
         
--  Replaces the specified value (by index) in a List
--  with a newly provided value
replaceAt index value (x:xs)
    | index == 0 = value:xs
    | otherwise = x:replaceAt (index-1) value xs
    
--  Is a slot (column) index of a board open in that it can hold an additional disc?
--  If a slot contains a 0 then we can place a token
isSlotOpen :: [[Int]] -> Int -> Bool
isSlotOpen bd index = elem 0 column
    where column = bd !! index       
    
--  Returns the number of columns of a board bd.
numSlot :: [[Int]] -> Int
numSlot bd = length bd

--  Returns the number of rows of a board bd
slotHeight :: [[Int]] -> Int
slotHeight bd
    -- Height is zero if the board is empty
    | (length bd == 0) = 0
    -- Count the number of elements in the first column
    | otherwise = length (bd !! 0) 
    
--  Gets the total amount of spaces available in the board bd
getTotal :: [[Int]] -> Int
getTotal bd = (numSlot bd) * (slotHeight bd)

--  Is the given board bd full in that there is no empty place?
--  We flatten the board and check if there are are no empty spaces
--      If there are no empty spaces then the board is full
isFull :: [[Int]] -> Bool
isFull bd = not (elem 0 (concat bd))

-- Wrapper function to check if player won with given board
isWonBy :: [[Int]] -> Int -> Bool
isWonBy bd player = isWonByRecursive bd player 0

--  Is the game played on a board bd won by a specified player?
isWonByRecursive :: [[Int]] -> Int -> Int -> Bool
isWonByRecursive bd player spot
    | spot == (getTotal bd) = False
    | isWinAt bd player spot = True
    | otherwise = isWonByRecursive bd player (spot+1)
    
--  Check if the specified player won at the specified place
isWinAt :: [[Int]] -> Int -> Int -> Bool
isWinAt bd player index
    -- We finished the search. The player didn't win.
    | outOfBounds bd col row = False
    -- Check the current token
    | (not (getTokenAt bd col row == player)) = False
    -- Check if they have 3 more tokens in any direction
    | (isWon == True) = True
    -- Check the next spot in the board
    | otherwise = isWinAt bd player (index + 1)
    where col = indexToCol bd index;
          row = indexToRow bd index;
          -- Count the number of tokens in each direction. Max of 4
          rightTokenCount = count bd col row 1 0 player 4;
          leftTokenCount = count bd col row (-1) 0 player 4;
          upTokenCount = count bd col row 0 1 player 4;
          downTokenCount = count bd col row 0 (-1) player 4;
          leftDiagonalUpTokenCount = count bd col row (-1) 1 player 4;
          leftDiagonalDownTokenCount = count bd col row (-1) (-1) player 4;
          rightDiagonalUpTokenCount = count bd col row 1 1 player 4;
          rightDiagonalDownTokenCount = count bd col row 1 (-1) player 4;
          -- Check if they have 3 tokens in any direction
          isWon = (leftTokenCount == 3) || (rightTokenCount == 3) ||
                  (upTokenCount == 3) || (downTokenCount == 3) || 
                  (leftDiagonalUpTokenCount == 3) || 
                  (leftDiagonalDownTokenCount == 3) ||
                  (rightDiagonalUpTokenCount == 3) || 
                  (rightDiagonalDownTokenCount == 3);
          
--  Count the number of tokens belonging to the specified player
--  From the specified column and row in the specified direction
--  Up to a maximum of (maxCount - 1)
count bd col row colDir rowDir player maxCount
    -- The search is finished
    | (maxCount == 1) = 0
    -- We found the player's token, count it
    | (getTokenAt bd newCol newRow == player) = 1 + recursiveCall
    -- Keep searching for the player's token
    | otherwise = recursiveCall
    where newCol = (col + colDir);
          newRow = (row + rowDir)
          recursiveCall = count bd newCol newRow colDir rowDir player (maxCount - 1)

--  Return a string representation of a board bd. It is a
--  higher-order function. The first argument (playerToChar) is a
--  function to convert a player to a character representation
boardToStr :: [[Int]] -> (Int -> String) -> String
boardToStr bd playerToChar = boardToStrRecursive bd playerToChar 0

--  The internal function for converting a board bd to a string representation
--  Keeps track of the current board position (index)
boardToStrRecursive :: [[Int]] -> (Int -> String) -> Int -> String
boardToStrRecursive bd playerToChar index
    -- Add a newline when we go past the last column
    | (col == (totalColumns - 1)) = tokenToStr ++ newLine ++ recursiveCall
    -- Stop when we go through all the tokens
    | (index == totalTokens) = ""
    -- Add the newly converted token
    | otherwise = tokenToStr ++ recursiveCall
    where recursiveCall = boardToStrRecursive bd playerToChar (index + 1);
          totalColumns = numSlot bd;
          totalTokens = getTotal bd;
          isLastRow = index == (totalTokens - 1);
          -- Add a newline except when it's the last row.
          newLine = if (isLastRow) then "" else "\n";
          col = indexToCol bd index;
          row = indexToRow bd index;
          -- Get the token and convert it with the playerToChar function
          token = getTokenAt bd col row
          tokenToStr = playerToChar token
  
--  Gets the token from the specified board bd at the specified location (col, row)
--  Returns -1 if the location is out of bounds
getTokenAt :: [[Int]] -> Int -> Int -> Int
getTokenAt bd col row 
    | outOfBounds bd col row = -1
    | otherwise = (bd !! col) !! row
    
--  (Boolean) Checks if the specified location in board bd is out of bounds
outOfBounds :: [[Int]] -> Int -> Int -> Bool
outOfBounds bd col row 
    | col < 0 = True
    | row < 0 = True
    | col >= numSlot bd = True
    | row >= slotHeight bd = True
    | otherwise = False
    
-- Converts an index to a column number of board bd
indexToCol :: [[Int]] -> Int -> Int
indexToCol bd index = mod index (numSlot bd)

-- Converts an index to a row number of board bd
indexToRow :: [[Int]] -> Int -> Int
indexToRow bd index = quot index ((slotHeight bd) + 1)