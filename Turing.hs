module Turing
( Action(..)
, Transition(..)
, TuringMachine(..)
) where

import Data.HashMap.Lazy (HashMap(..))
import Tape ( Action(..)
            , Tape(..)
            , fromString
            )


data Transition = Transition { read :: Char
                             , toState :: String
                             , write :: Char
                             , action :: Action
                             } deriving (Show)

data TuringMachine = TuringMachine { name :: String
                                   , alphabet :: [Char]
                                   , blank :: Char
                                   , states :: [String]
                                   , initial :: String
                                   , finals :: [String]
                                   , transitions :: HashMap String [Transition]
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
