module TryPrintErr (
  catch,
  printErr,
) where

import Control.Exception (catch)
import Control.Exception (SomeException)

printErr :: SomeException -> IO ()
printErr e = putStrLn $ "Error: " ++ show e
