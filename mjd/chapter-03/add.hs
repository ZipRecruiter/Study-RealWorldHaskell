-- file: ch03/add.hs
sumList (x:xs) = x + sumList xs
sumList []     = 0
