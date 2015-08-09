import qualified Data.ByteString.Lazy as BS
import System.Environment (getArgs)

import Deserializer (parseTMachine)
import Turing (runMachine)

-- Extract the arguments
extractArgs :: [String] -> (String, String)
extractArgs [x, y] = (x, y)
extractArgs _ = error "Not the right number of arguments."

main :: IO ()
main = do
    { args <- getArgs
    ; let (path, input) = extractArgs args
    ; machine <- (fmap parseTMachine . BS.readFile) path
    ; let output = fmap (flip runMachine input) machine
    ; case output of
        Just s -> putStrLn s
        Nothing -> putStrLn "Error when parsing the json file."
    }
