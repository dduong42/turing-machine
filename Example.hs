module Example (unarySub) where

import Data.HashMap.Lazy ( HashMap(..)
                         , fromList
                         )

import Turing ( TuringMachine(..)
              , TransitionAction(..)
              , Action(..)
              )


unarySub :: TuringMachine
unarySub = TuringMachine
    "unary_sub"
    ['1', ' ', '-', '=']
    ' '
    ["scanright", "eraseone", "subone", "skip", "HALT"]
    "scanright"
    ["HALT"]
    (fromList [ ("scanright", fromList [ (' ', (TransitionAction "scanright" ' ' MoveRight))
                                       , ('1', (TransitionAction "scanright" '1' MoveRight))
                                       , ('-', (TransitionAction "scanright" '-' MoveRight))
                                       , ('=', (TransitionAction "eraseone" ' ' MoveLeft))
                                       ])
              , ("eraseone",  fromList [ ('1', (TransitionAction "subone" '=' MoveLeft))
                                       , ('-', (TransitionAction "HALT" ' ' MoveLeft))
                                       ])
              , ("subone",    fromList [ ('1', (TransitionAction "subone" '1' MoveLeft))
                                       , ('-', (TransitionAction "skip" '-' MoveLeft))
                                       ])
              , ("skip",      fromList [ (' ', (TransitionAction "skip" ' ' MoveLeft))
                                       , ('1', (TransitionAction "scanright" ' ' MoveRight))
                                       ])
              ])
