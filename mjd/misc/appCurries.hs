

appFst f g x y = g (f x) y
appSnd f g x y = g x (f y)
cFst x y = x
cSnd x y = y

-- myLength :: Foldable f => f a -> Int
-- myLength = foldr (cSnd . appSnd (+ 1)) 0
