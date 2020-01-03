any' f = foldr (\x a -> a || f x ) False

cycle' xs = foldr step [] xs
  where
  step x a
    | (length a) `mod` (length xs) == 0 = a ++ cycle xs
    | otherwise = x:a

words' 

test = do
  print "any"
  print $ any' odd [1..5]
  print $ any' even [1,3..5]

  print $ words' 

  --WIP cycle' loops forever
  --print "cycle"
  --print $ take 10 $ cycle' [1..3]
