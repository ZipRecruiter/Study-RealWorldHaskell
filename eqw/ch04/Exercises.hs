-- file: ch04/Exercises.hs
-- Solutions to Real World Haskell, Chapter 4
module ChapterFour where

-- 1. Write your own "safe" definitions of the standard partial list
--    functions, but make sure that yours never fail. As a hint, you
--    might want to consider using the following types. (Included. —EQW)
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast = safeHead . reverse

safeInit :: [a] -> Maybe [a]
-- I'm sure there's a better way to do this,
-- but I'm mostly just playing around.
safeInit l = reverse <$> (safeTail $ reverse l)

-- 2. Takes a predicate and a list of any type and splits its input
--    list on every element for which the predicate returns `False`.
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f l  = [x] ++ splitWith f (dropWhile f xs)
  where (x, xs) = break f (dropWhile f l)
