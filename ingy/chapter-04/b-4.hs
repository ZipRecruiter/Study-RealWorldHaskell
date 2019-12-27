takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | not $ f x = []
  | otherwise = x:(takeWhile' f xs)

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f xs = foldr step [] xs
  where
    step x a
      | not $ f x = []
      | otherwise = x:a

test = do
  print $ takeWhile' (/=' ') "foo bar"
  print $ takeWhile' (/=' ') " foo bar"
  print $ takeWhile' (/=' ') ""

  print $ takeWhile'' (/=' ') "foo bar"
  print $ takeWhile'' (/=' ') " foo bar"
  print $ takeWhile'' (/=' ') ""
