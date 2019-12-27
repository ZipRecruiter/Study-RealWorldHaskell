takeWhileR :: (a -> Bool) -> [a] -> [a]
takeWhileR _ [] = []
takeWhileR f (x:xs) = (if f x then [x] ++ takeWhileR f xs else [])

takeWhileF :: Foldable t => (a -> Bool) -> t a -> [a]
takeWhileF f x = foldr tWF' [] x
                 where
                  tWF' x c = if f x then x : c else []
