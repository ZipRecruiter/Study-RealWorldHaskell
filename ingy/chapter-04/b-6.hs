any' f = foldr (\x a -> f x || a) False
cycle' xs = foldr (\x a -> xs ++ a) [] [1..]
words' = filter (/="") . foldr step [""] where
  step c (w:ws) = if c `elem` " \t\n" then "":w:ws else (c:w):ws
unlines' = foldr (\l a -> l ++ "\n" ++ a) ""


test = do
  putStrLn "# any'"
  print $ any' (<0) [1..3]
  print $ any' (>0) [1..]
  print $ any' (>0) []
  print $ any' (<0) [1..3]

  putStrLn "\n# cycle'"
  print $ take 15 $ cycle' ['t']
  print $ take 15 $ cycle' "abc"
  print $ take 15 $ cycle' [1,2,3]

  putStrLn "\n# words'"
  print $ words' ""
  print $ words' "foo"
  print $ words' "I like pie"
  print $ words' "  I  like  pie  "

  putStrLn "\n# unlines'"
  print $ unlines' []
  print $ unlines' ["foo"]
  print $ unlines' ["foo", "bar"]
  print $ unlines' ["foo", "", "bar"]
