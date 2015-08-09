module Turing
( Action(..)
, Transition(..)
, TuringMachine(..)
) where

import Data.HashMap.Lazy (HashMap(..))
import Tape(Action(..))


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
