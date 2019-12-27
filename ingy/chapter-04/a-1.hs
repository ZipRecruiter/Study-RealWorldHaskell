safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

safeTail :: [a] -> Maybe [a]
safeTail xs = if null xs then Nothing else Just $ tail xs

safeLast :: [a] -> Maybe a
safeLast xs =
  if null xs
  then Nothing
  else (Just . last) xs

safeInit :: [a] -> Maybe [a]
safeInit xs
  | null xs = Nothing
  | otherwise = Just . init $ xs


test = do
  print $ safeHead [42..]
  print $ safeHead ""
  print $ safeTail "Fred"
  print $ safeTail ""
  print $ safeLast "Fred"
  print $ safeLast ""
  print $ safeInit "Freed"
  print $ safeInit ""
