-- {-# LANGUAGE NoMonomorphismRestriction #-}

myLength0 ls = foldr (const (+ 1)) 0 ls

myLength1 :: [a] -> Int
myLength1 ls = foldr (const (+ 1)) 0 ls

myLength2 :: Foldable f => f a -> Int
myLength2 ls = foldr (const (+ 1))  0 ls

myLength3 :: (Foldable f, Integral b) => f a -> b
myLength3 ls = foldr (const (+ 1))  0 ls

-- myLength4 :: (Foldable t, Num b) => t a -> b
myLength4 = foldr (const (+ 1))  0

myLength5 :: [a] -> Int
myLength5 = foldr (const (+ 1))  0

myLength6 :: Foldable f => f a -> Int
myLength6 = foldr (const (+ 1))  0

myLength7 :: (Foldable f, Integral b) => f a -> b
myLength7 = foldr (const (+ 1))  0


