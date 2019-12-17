
intersperse sep [] = ""
intersperse sep (a:rest) 
  | rest == []    =  a
  | otherwise     =  a ++ sep ++ intersperse sep rest


-- doesn't work
-- intersperse2 sep ls = foldr (\a b -> a ++ sep ++ b) "" ls

-- fails on empty list
-- perlmap f ls = concat $ map f ls
-- intersperse3 sep ls = concat $ tail $ perlmap (\x -> [ sep, x ]) ls

-- works on nonempty lists
intersperse4 sep = foldl1 (\a b -> a ++ sep ++ b)
