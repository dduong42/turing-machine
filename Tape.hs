module Tape
( Action(..)
, Tape(..)
, moveHead
, moveLeft
, moveRight
, newTape
) where


data Action = MoveLeft | MoveRight deriving (Show)

data Tape = Tape { tapeContent :: [Char]
                 , tapeHead :: Int
                 , blankCharacter :: Char
                 } deriving (Show)

-- Create a new tape from a blank character
newTape :: Char -> Tape
newTape blankChar = Tape [blankChar] 0 blankChar

-- Modify a tape by applying a function to the content of a tape
modifyContent :: ([Char] -> [Char]) -> Tape -> Tape
modifyContent f (Tape tcontent thead tblank) = Tape (f tcontent) thead tblank

-- Modify a tape by applying a function to the head of a tape
modifyHead :: (Int -> Int) -> Tape -> Tape
modifyHead f (Tape tcontent thead tblank) = Tape tcontent (f thead) tblank

actionToInt :: Action -> Int
actionToInt MoveLeft = -1
actionToInt MoveRight = 1

addAction :: Action -> Int -> Int
addAction action index = index + (actionToInt action)

-- Tell us if we need to extend the tape
needToExtend :: Action -> Tape -> Bool
needToExtend action tape = not (0 <= addResult && addResult < length (tapeContent tape))
    where addResult = addAction action (tapeHead tape)

-- Return the new head after executing an action
newHead :: Action -> Int -> Int
newHead action thead = max 0 (addAction action thead)

-- Extend the content list to the left
extendLeft :: Char -> [Char] -> [Char]
extendLeft blank content = blank:content

-- Extend the content list to the right
extendRight :: Char -> [Char] -> [Char]
extendRight blank content = content ++ [blank]

-- Return the extension function from a direction
extendFunction :: Action -> Char -> ([Char] -> [Char])
extendFunction MoveLeft blank = extendLeft blank
extendFunction MoveRight blank = extendRight blank

-- Apply a function to an argument if a condition is True, otherwise return
-- the original value
applyIf :: (a -> a) -> Bool -> a -> a
applyIf f cond x = if cond then f x else x

-- Move the head to a specific direction
moveHead :: Action -> Tape -> Tape
moveHead action tape = (modifyHead (newHead action) . modifyContent contentFunction) tape
    where contentFunction = applyIf (extendFunction action (blankCharacter tape)) (needToExtend action tape)

-- Move the tape to the right
moveRight :: Tape -> Tape
moveRight = moveHead MoveRight

-- Move the tape to the left
moveLeft :: Tape -> Tape
moveLeft = moveHead MoveLeft

-- Read the current element pointed by the head
readTape :: Tape -> Char
readTape tape = tapeContent tape !! (tapeHead tape)

-- Replace the ith element of a list
replaceIth :: a -> Int -> [a] -> [a]
replaceIth element 0 (x:xs) = element:xs
replaceIth element index (x:xs) = x:(replaceIth element (index - 1) xs)

-- Write a char in the tape
writeTape :: Char -> Tape -> Tape
writeTape c tape = modifyContent (replaceIth c (tapeHead tape)) tape
