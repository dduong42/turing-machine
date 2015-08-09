module Turing
( Action(..)
, TransitionAction(..)
, TuringMachine(..)
, runMachine
) where

import Data.HashMap.Lazy ( HashMap(..)
                         , (!)
                         )

import Data.List (intercalate)

import Tape ( Action(..)
            , Tape(..)
            , fromString
            , moveHead
            , readTape
            , representation
            , writeTape
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
                                       , instanceState :: String
                                       , instanceTape :: Tape
                                       } deriving (Show)


-- Create an instance of a machine
createInstance :: TuringMachine -> String -> MachineInstance
createInstance machine input = MachineInstance machine
                                               (initial machine)
                                               (fromString (blank machine) input)

-- Tell if a state is a final state
isFinalState :: String -> TuringMachine -> Bool
isFinalState state machine = elem state (finals machine)

-- Tell if an instance is in a final state
isInstanceInFinalState :: MachineInstance -> Bool
isInstanceInFinalState minstance
    = isFinalState (instanceState minstance) (turingMachine minstance)

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

-- Return the next tape of the machine instance
nextTape :: MachineInstance -> Tape
nextTape minstance
    = let taction = actionForInstance minstance
          currentTape = instanceTape minstance
          direction = action taction
          newChar = write taction
      in ((moveHead direction) . (writeTape newChar)) currentTape

-- Return the next instance
nextInstance :: MachineInstance -> MachineInstance
nextInstance minstance
    | isInstanceInFinalState minstance = minstance
    | otherwise = ((modifyState stateFunction) . (modifyTape tapeFunction)) minstance
    where stateFunction = \_ -> nextState minstance
          tapeFunction = \_ -> nextTape minstance

-- Return the representation of an instance (it's the representation of its tape)
instanceRepresentation :: MachineInstance -> String
instanceRepresentation = representation . instanceTape

-- Run an instance
runInstance :: MachineInstance -> [MachineInstance]
runInstance minstance
    | isInstanceInFinalState minstance = [minstance]
    | otherwise = minstance:((runInstance . nextInstance) minstance)

-- Return what will be displayed by the program
instanceOutput :: MachineInstance -> String
instanceOutput = intercalate "\n" . (fmap instanceRepresentation) . runInstance

-- Run a turing machine
runMachine :: TuringMachine -> String -> String
runMachine tmachine input = instanceOutput (createInstance tmachine input)
