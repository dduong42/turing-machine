{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.ByteString.Lazy as BS
import Data.HashMap.Lazy ( HashMap(..)
                         )
import Data.Aeson
import qualified Turing as T

data TransitionInput = TransitionInput { action ::String
                                       , read :: String
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
        ; read <- o .: "read"
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
