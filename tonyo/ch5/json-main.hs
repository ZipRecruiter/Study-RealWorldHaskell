import YAJP7
import System.Environment (getArgs)
import Text.Printf
import System.Exit

isErr :: JVal -> Bool
isErr (JErr _) = True
isErr _ = False

main = do
  args <- getArgs
  case args of
    [file] -> do
                fdata <- readFile file
                pres <- yajp7_parse fdata
                exitWith $ if isErr pres then ExitFailure 1 else ExitSuccess 
    _      -> exitWith $ ExitFailure 2
