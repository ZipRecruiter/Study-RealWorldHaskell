transpose text = unlines ts
  where
    ts = replicate x $ show y
    x = length $ ls
    y = maximum $ map length ls
    ls = lines text
