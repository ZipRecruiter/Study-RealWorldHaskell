concat' :: [[a]] -> [a]
concat' = foldr (++) []

test = do
  print $ concat' ["foo", "bar", "baz"]
  print $ concat' ["foo", "", "bar", "baz"]
  print $ concat' ["foo"]
  print $ concat' [""]
  print $ concat' [[0]]
  print $ concat' [[0],[]]
