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
    (fromList [ (("scanright", ' '), (TransitionAction "scanright" ' ' MoveRight))
              , (("scanright", '1'), (TransitionAction "scanright" '1' MoveRight))
              , (("scanright", '-'), (TransitionAction "scanright" '-' MoveRight))
              , (("scanright", '='), (TransitionAction "eraseone" ' ' MoveLeft))
              , (("eraseone", '1'), (TransitionAction "subone" '=' MoveLeft))
              , (("eraseone", '-'), (TransitionAction "HALT" ' ' MoveLeft))
              , (("subone", '1'), (TransitionAction "subone" '1' MoveLeft))
              , (("subone", '-'), (TransitionAction "skip" '-' MoveLeft))
              , (("skip", ' '), (TransitionAction "skip" ' ' MoveLeft))
               ,(("skip", '1'), (TransitionAction "scanright" ' ' MoveRight))
              ])
