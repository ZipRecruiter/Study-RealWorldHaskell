-- file: ch04/Interact.hs
module Interact where

import Data.List (transpose)
import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

-- 3. Using the command framework from the section called "A simple
--    command line framework", write a program that prints the first
--    word of each line of its input.
firstWordOfEachLine :: String -> String
firstWordOfEachLine = unlines . map head . map words . lines

-- 4. Write a program that transposes the text in a file.
--    For instance, it should convert "hello\nworld\n" to
--    "hw\neo\nlr\nll\nod\n".
transposeText :: String -> String
transposeText = unlines . transpose . lines

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = transposeText
