groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f = foldr step []
  where
    step x [] = [[x]]
    step x (y:xs) =
      if f x (head y) then (x:y):xs else [x]:y:xs


test = do
  print $ groupBy' (==) ""
  print $ groupBy' (==) "a"
  print $ groupBy' (==) "abc"
  print $ groupBy' (==) "aabbbcc"
  print $ groupBy' (==) [1..5]
  print $ groupBy' (\x y -> x + y > 5) [1..5]
