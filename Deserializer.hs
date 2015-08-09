{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Deserializer
( parseTMachine
) where

import qualified Data.ByteString.Lazy as BS
import Data.HashMap.Lazy ( HashMap(..)
                         , fromList
                         , toList
                         )
import Data.Aeson
import qualified Turing as T

data TransitionInput = TransitionInput { action ::String
                                       , read' :: String
                                       , toState :: String
                                       , write :: String
                                       } deriving (Show)

data MachineInput = MachineInput { name :: String
                                 , alphabet :: [String]
                                 , blank :: String
                                 , states :: [String]
                                 , initial :: String
                                 , finals :: [String]
                                 , transitions :: HashMap String [TransitionInput]
                                 } deriving (Show)

instance FromJSON TransitionInput where
    parseJSON = withObject "TransitionInput" $ \o -> do
        { action <- o .: "action"
        ; read' <- o .: "read"
        ; toState <- o .: "to_state"
        ; write <- o .: "write"
        ; return TransitionInput{..}
        }

instance FromJSON MachineInput where
    parseJSON = withObject "MachineInput" $ \o -> do
        { name <- o .: "name"
        ; alphabet <- o .: "alphabet"
        ; blank <- o .: "blank"
        ; states <- o .: "states"
        ; initial <- o .: "initial"
        ; finals <- o .: "finals"
        ; transitions' <- o .: "transitions"
        ; transitions <- parseJSON transitions'
        ; return MachineInput{..}
        }

-- Turn a string into an Action
stringToAction :: String -> T.Action
stringToAction "LEFT" = T.MoveLeft
stringToAction "RIGHT" = T.MoveRight
stringToAction _ = error "Unknown action"

-- Turn a TransitionInput into a TransitionAction
inputToAction :: TransitionInput -> T.TransitionAction
inputToAction (TransitionInput taction tread ttoState twrite)
    = T.TransitionAction ttoState (head twrite) (stringToAction taction)

inputToTransitions1 :: String -> TransitionInput -> ((String, Char), T.TransitionAction)
inputToTransitions1 s input = ((s, (head . read') input), inputToAction input)

inputToTransitions2 :: (String, [TransitionInput]) -> [((String, Char), T.TransitionAction)]
inputToTransitions2 (s, xs) = fmap (inputToTransitions1 s) xs

inputToTransitions :: HashMap String [TransitionInput] -> HashMap (String, Char) T.TransitionAction
inputToTransitions = fromList . concat . (fmap inputToTransitions2) . toList

-- Return a TuringMachine for a MachineInput
inputToTMachine :: MachineInput -> T.TuringMachine
inputToTMachine (MachineInput mname malpha mblank mstates minit mfinals mtransitions)
    = T.TuringMachine mname (concat malpha) (head mblank) mstates minit
                      mfinals (inputToTransitions mtransitions)

-- Parse a Turing Machine from a ByteString
parseTMachine :: BS.ByteString -> Maybe T.TuringMachine
parseTMachine = (fmap inputToTMachine) . decode
