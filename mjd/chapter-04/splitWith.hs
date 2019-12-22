
-- splitWith :: (a -> Bool) -> [a] -> [[a]]

-- splitWith f ls = splitWith' ls [] []
--   where splitWith' []     last acc = reverse last : acc
--         splitWith' (x:xs) last acc
--           | f x =        splitWith' xs [] (reverse last :acc)
--           | otherwise =  splitWith' xs (x:last) acc


-- pull one chunk off the front of a list
chunk f [] = ([], [])
chunk f (x:xs)
  | f x  = ([], xs)
  | otherwise = let (ch, rest) = chunk f xs
                 in (x:ch, rest)

-- This works, except it doesn't have the discard-empty-chunks feature
-- Easy to add that as a second pass though as in the next function
splitWith1 f [] = []
splitWith1 f ls = let (first, more) = chunk f ls in first : splitWith1 f more

-- Combine the two previous into a single function
-- I was hoping that there would be opportunities to simplify the code
-- but I couldn't find any
splitWith2 f [] = []
splitWith2 f ls = let (first, more) = chunk f ls
              in filter (not . null) $ first : splitWith2 f more
  where
    chunk f [] = ([], [])
    chunk f (x:xs)
      | f x  = ([], xs)
      | otherwise = let (ch, rest) = chunk f xs
                    in (x:ch, rest)

-- I think this one works perfectly.  But so much code!
splitWith :: (Char -> Bool) -> String -> [String]
splitWith f ls
  | null unsplit_part   = if null first_chunk then [] else [first_chunk]
  | null first_chunk    = splitWith f (dropWhile f unsplit_part)
  | otherwise           = first_chunk : splitWith f (tail unsplit_part)
  where
    first_chunk  = takeWhile (not . f) ls
    unsplit_part = dropWhile (not . f) ls

-- Failed attempts at writing really simple unit tests
try a x
  | a == x    = True
  | otherwise = error $ "Failed: actual=" ++ (show a) ++ "; expected=" ++ (show x)

try_splitWith arg x = try (splitWith (== ' ') arg) x



-- tests = [
--   try_splitWith "" = []
--   try_splitWith "Word"    ["Word"]  ,
--   try_splitWith " Word"   ["Word"]  ,
--   try_splitWith "  Word"  ["Word"]  ,
--   try_splitWith "Word "   ["Word"]  ,
--   try_splitWith "Word  "  ["Word"]  ,
--   try_splitWith " Word "  ["Word"]  ,
-- ]


-- chunkList f [] = []
-- chunkList f ls = 
-- splitWith f ls = foldr
-- splitWith f (x:xs)
--   | f x       =     splitWith f xs
--   | otherwise = x : splitWith f xs
