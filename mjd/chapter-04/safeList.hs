
safe _ [] = Nothing
safe f ls = Just $ f ls

safeHead :: [a] -> Maybe a
safeHead = safe head

safeTail :: [a] -> Maybe [a]
safeTail = safe tail

safeLast :: [a] -> Maybe a
safeLast = safe last

safeInit :: [a] -> Maybe [a]
safeInit = safe init
