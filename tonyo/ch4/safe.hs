import Data.Maybe

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail _ = Nothing

safeLast :: [a] -> Maybe a
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs
safeLast _ = Nothing

safeInit :: [a] -> Maybe [a]
safeInit (x:[]) = Just []
safeInit (x:xs) = Just $ x : (fromMaybe [] (safeInit xs))
safeInit _ = Nothing
