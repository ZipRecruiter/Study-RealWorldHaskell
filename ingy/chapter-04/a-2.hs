splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs = dropWhile null $ splitWith' f xs
  where
  splitWith' _ [] = []
  splitWith' g ys = word:(splitWith' g rest)
    where
    word = takeWhile (not.g) ys
    rest = dropWhile g $ drop (length word + 1) ys


-- Run the test suite with:
-- ./bin/ghcr ingy/chapter-04/a-2.hs test
test = do
  print $ splitWith (==' ') ""
  print $ splitWith (==' ') " "
  print $ splitWith (==' ') "    "
  print $ splitWith (==' ') "foo"
  print $ splitWith (==' ') "  foo  "
  print $ splitWith (==' ') "foo bar baz"
  print $ splitWith (==' ') "foo  bar   baz"
  print $ splitWith (==' ') "   foo  bar   baz   "
