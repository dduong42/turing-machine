import Data.HashMap.Lazy ( HashMap(..)
                         , fromList
                         )

import Turing ( TuringMachine(..)
              , Transition(..)
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
    (fromList [ ("scanright", [ Transition ' ' "scanright" ' ' MoveRight
                              , Transition '1' "scanright" '1' MoveRight
                              , Transition '-' "scanright" '-' MoveRight
                              , Transition '=' "eraseone" ' ' MoveLeft
                              ])
              , ("eraseone",  [ Transition '1' "subone" '=' MoveLeft
                              , Transition '-' "HALT" ' ' MoveLeft
                              ])
              , ("subone",    [ Transition '1' "subone" '1' MoveLeft
                              , Transition '-' "skip" '-' MoveLeft
                              ])
              , ("skip",      [ Transition ' ' "skip" ' ' MoveLeft
                              , Transition '1' "scanright" ' ' MoveRight
                              ])
              ])
