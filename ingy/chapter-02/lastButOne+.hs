{-
  Usage:
    runghc lastButOne+.hs 1 39999999
    runghc lastButOne+.hs 2 39999999
    runghc lastButOne+.hs 3 39999999
    runghc lastButOne+.hs 4 39999999
    runghc lastButOne+.hs 5 39999999
    runghc lastButOne+.hs 6 39999999
    runghc lastButOne+.hs 7 39999999
-}

import System.Environment (getArgs)
import Text.Printf (printf)
import System.Exit --(exitWith)

-- Install with: `cabal install criterion`
import Criterion.Measurement (getTime)


-- Run a given lastButOne implementation and show how long it took:
main = do
  [which, len] <- getArgs

  let num = read len :: Int
  let try name func = printf "%s %d = %d\n" name num $ func [1..num]

  start <- getTime

  case which of
    "1" -> try "ingy-1" lastButOne1
    "2" -> try "ingy-2" lastButOne2
    "3" -> try "df-1" lastButOne3
    "4" -> try "df-2" lastButOne4
    "5" -> try "eqw" lastButOne5
    "6" -> try "tonyo-1" lB1
    "7" -> try "tonyo-2" lB1'
    otherwise -> do
      printf "Error: unknown algorithm number '%s'\n" which
      exitWith $ ExitFailure 1

  stop <- getTime
  printf "%.2f secs" $ stop - start


-- Functional implementations:

-- ingy-1
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

-- ingy-2
lastButOne2 :: [a] -> a
lastButOne2 [] =
  error "List is empty"
lastButOne2 [x] =
  error "List only has 1 item"
lastButOne2 [x,y] = x
lastButOne2 (x:xs) = lastButOne2 xs

-- df-1
lastButOne3 :: [a] -> a
lastButOne3 (x:(y:ys)) = if null ys then x else lastButOne3 (y:ys)
lastButOne3 [x]        = error ("lastButOne: list must have 2 or more elements")
lastButOne3 []         = error ("lastButOne: list cannot be empty")

-- df-2
lastButOne4 :: [a] -> a
lastButOne4 xs =  head (tail (reverse xs))

-- eqw
lastButOne5 :: [a] -> a
lastButOne5 xs = xs!!(length xs - 2)

-- tonyo-1
lB1 :: [a] -> a
lB1 []  = error "Not enough elements"
lB1 [a] = error "Not enough elements"
lB1 xs  = if lB1R_ xs 10
          then error "Infinite-ish list, won't continue"
          else xs !! ((length xs) - 2)

lB1R_ :: [a] -> Int -> Bool
lB1R_ [] _ = False
lB1R_ xs x = if x > 0
             then lB1R_ (drop 65000 xs) (x - 1)
             else True

-- tonyo-2
lB1' :: [a] -> a
lB1' []  = error "Not enough elements"
lB1' [a] = error "Not enough elements"
lB1' xs  = xs !! ((length xs) - 2)
