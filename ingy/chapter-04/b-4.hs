takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) =
  if f x then x:(takeWhile' f xs) else []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f = foldr step []
  where step x a = if f x then x:a else []

test = do
  print $ takeWhile' (/=' ') "foo bar"
  print $ takeWhile' (/=' ') " foo bar"
  print $ takeWhile' (/=' ') ""

  print $ takeWhile'' (/=' ') "foo bar"
  print $ takeWhile'' (/=' ') " foo bar"
  print $ takeWhile'' (/=' ') ""
