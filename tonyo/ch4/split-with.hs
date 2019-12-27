splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ []   = []
splitWith f (xs) = let (fore, aft) = break f (dropWhile f xs)
                   in [fore] ++ (splitWith f (dropWhile f aft))
