module Turing
( Action(..)
, TransitionAction(..)
, TuringMachine(..)
) where

import Data.HashMap.Lazy (HashMap(..))
import Tape ( Action(..)
            , Tape(..)
            , fromString
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
                                       , state :: String
                                       , tape :: Tape
                                       } deriving (Show)


-- Create an instance of a machine
createInstance :: TuringMachine -> String -> MachineInstance
createInstance machine input = MachineInstance machine
                                               (initial machine)
                                               (fromString (blank machine) input)
