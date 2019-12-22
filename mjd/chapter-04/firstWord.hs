
module Main where

import Interact(mainWith, mainWithLines)

-- this worked
-- main = mainWith (unlines . map (head . words) . lines)

main = mainWithLines (head . words)
