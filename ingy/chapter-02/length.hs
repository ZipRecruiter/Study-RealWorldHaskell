import Data.List

import Text.Printf (printf); import Criterion.Measurement (getTime)
time f = do start <- getTime; print f; stop <- getTime; printf "%.2f secs\n" $ stop - start

main = do
  let list = [1..39999999]
  time $ length2 list
  time $ length2 list
  time $ length2 list
  time $ length2 list

length1 = foldl' (+) 0 . map (const 1)
length2 = sum . map (const 1)
length3 [] = 0
length3 (_:xs) = 1 + length3 xs
--length4 = length4' 0
    --where length4' a [] = a
          --length4' a (_:xs) = length4' (a+1) xs
