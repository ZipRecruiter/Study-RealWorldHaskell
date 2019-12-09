{-
  Usage:
    runghc lastButOne.hs 1 29999999
    runghc lastButOne.hs 2 29999999
-}

import System.Environment (getArgs)
import Text.Printf (printf)
import System.Exit --(exitWith)

-- cabal install criterion
import Criterion.Measurement (getTime)

main = do
  [which, len] <- getArgs
  let num = read len :: Int

  start <- getTime

  case which of
    "1" -> printf "lastButOne1 %d = %d\n"
           num $ lastButOne1 [1..num]
    "2" -> printf "lastButOne2 %d = %d\n"
           num $ lastButOne2 [1..num]
    otherwise -> do
      printf "Error: unknown algorithm number '%s'\n" which
      exitWith $ ExitFailure 1

  stop <- getTime
  printf "%.2f secs" $ stop - start

lastButOne1 :: [a] -> a
lastButOne1 xs
  | len == 0 =
    error "List is empty"
  | len == 1 =
    error "List only has 1 item"
  | otherwise =
    head $ drop (len - 2) xs
  where
    len = length xs

lastButOne2 :: [a] -> a
lastButOne2 [] =
  error "List is empty"
lastButOne2 [x] =
  error "List only has 1 item"
lastButOne2 [x,y] = x
lastButOne2 (x:xs) = lastButOne2 xs
