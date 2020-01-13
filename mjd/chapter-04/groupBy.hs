groupBy :: (a -> a -> Bool) -> [a] -> [[a]]

groupBy f ls = foldr ff [] ls where
   ff a [] = [[a]]
   ff a ((g:gs):groups) = if (f a g) then ((a:g:gs) : groups)
                          else ([a] : (g:gs) : groups)
                               
