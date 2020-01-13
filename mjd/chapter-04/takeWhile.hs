
myTakeWhile_1 :: (a -> Bool) -> [a] -> [a];
myTakeWhile_1 _ [] = []
myTakeWhile_1 f (a:as) = if f a then a : myTakeWhile_1 f as
                         else []


myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f as = foldr (\a b -> if f a then a:b else []) [] as

