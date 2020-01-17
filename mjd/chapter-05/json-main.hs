import JSON
import System.Environment (getArgs)
import Text.Printf
import System.Exit

main = do
  args <- getArgs
  case args of
    [file] -> do
                fdata <- readFile file
                -- putStrLn fdata
                case json_value fdata of
                  Just v -> putStrLn $ show v
                  Nothing -> exitWith $ ExitFailure 1
                exitWith ExitSuccess
                
    _      -> exitWith $ ExitFailure 2
