module Turing
( Action(..)
, TransitionAction(..)
, TuringMachine(..)
) where

import Data.HashMap.Lazy ( HashMap(..)
                         , (!)
                         )
import Tape ( Action(..)
            , Tape(..)
            , fromString
            , readTape
            )


data TransitionAction = TransitionAction { toState :: String
                                         , write :: Char
                                         , action :: Action
                                         } deriving (Show)

data TuringMachine = TuringMachine { name :: String
                                   , alphabet :: [Char]
                                   , blank :: Char
                                   , states :: [String]
                                   , initial :: String
                                   , finals :: [String]
                                   , transitions :: HashMap (String, Char) TransitionAction
                                   } deriving (Show)

data MachineInstance = MachineInstance { turingMachine :: TuringMachine
                                       , machineState :: String
                                       , machineTape :: Tape
                                       } deriving (Show)


-- Create an instance of a machine
createInstance :: TuringMachine -> String -> MachineInstance
createInstance machine input = MachineInstance machine
                                               (initial machine)
                                               (fromString (blank machine) input)

-- Tell if a state is a final state
isFinalState :: String -> TuringMachine -> Bool
isFinalState state machine = elem state (finals machine)

-- Modify an instance by applying a function to the state
modifyState :: (String -> String) -> MachineInstance -> MachineInstance
modifyState f (MachineInstance machine currentState currentTape)
    = MachineInstance machine (f currentState) currentTape

-- Modify an instance by applying a function to the tape
modifyTape :: (Tape -> Tape) -> MachineInstance -> MachineInstance
modifyTape f (MachineInstance machine currentState currentTape)
    = MachineInstance machine currentState (f currentTape)

-- Return the TransitionAction associated with the MachineInstance
actionForInstance :: MachineInstance -> TransitionAction
actionForInstance (MachineInstance machine currentState currentTape)
    = (transitions machine) ! (currentState, currentChar)
    where currentChar = readTape currentTape

-- Return the next state of the machine instance
nextState :: MachineInstance -> String
nextState = toState . actionForInstance
