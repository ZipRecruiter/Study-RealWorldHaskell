concatT :: [[a]] -> [a]
concatT xs = foldr concatT' [] xs
             where
               concatT' :: [a] -> [a] -> [a]
               concatT' a x = a ++ x
