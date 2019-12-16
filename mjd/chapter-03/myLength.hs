

myLength :: [a] -> Int
myLength [] = 0
myLength (_:as) = 1 + myLength as

myLength' :: [a] -> Int
myLength' = foldr (const (+ 1))  0

