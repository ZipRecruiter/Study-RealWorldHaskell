# Chapter 4 exercises

## Working with Lists

1. Write your own “safe” definitions of the standard partial list functions, but make sure that yours never fail.
   As a hint, you might want to consider using the following types.

   ```haskell
   -- Run the test suite with:
   -- ./bin/ghcr ingy/chapter-04/a-1.hs test

   -- file: ch04/ch04.exercises.hs
   safeHead :: [a] -> Maybe a
   safeTail :: [a] -> Maybe [a]
   safeLast :: [a] -> Maybe a
   safeInit :: [a] -> Maybe [a]
   ```

   ```
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
   ```

2. Write a function `splitWith` that acts similarly to `words`, but takes a predicate and a list of any type, and splits its input list on every element for which the predicate returns `False`.

   ```haskell
   -- file: ch04/ch04.exercises.hs
   splitWith :: (a -> Bool) -> [a] -> [[a]]
   ```

   ```
   -- Run the test suite with:
   -- ./bin/ghcr ingy/chapter-04/a-2.hs test

   splitWith :: (a -> Bool) -> [a] -> [[a]]
   splitWith f xs = dropWhile null $ splitWith' f xs
     where
     splitWith' _ [] = []
     splitWith' g ys = word:(splitWith' g rest)
       where
       word = takeWhile (not.g) ys
       rest = dropWhile g $ drop (length word + 1) ys
   ```

3. Using the command framework from the section called “A simple command line framework”, write a program that prints the first word of each line of its input.

   ```
   -- Run with:
   -- ./bin/ghcr -i ingy/chapter-04/a-3.hs firstWords < ingy/chapter-04/a-3.hs

   firstWords = unlines.(map firstWord).lines

   firstWord [] = []
   firstWord xs = (head.words) xs
   ```

4. Write a program that transposes the text in a file.
   For instance, it should convert `"hello\nworld\n"` to `"hw\neo\nlr\nll\nod\n"`.
   ```
   -- Run with:
   -- ./bin/ghcr -i ingy/chapter-04/a-4.hs transposeText < ingy/chapter-04/file1.txt

   transposeText = unlines.transpose.lines

   transpose xs = takeWhile (/="") $ map next [0..]
     where next i = foldr (++) "" $ map (take 1 . drop i) xs
   ```

## How to Think About Loops

1. Use a fold (choosing the appropriate fold will make your code much simpler) to rewrite and improve upon the `asInt` function from the section called “Explicit recursion”.

   Your function should behave as follows.

   ```
   ghci> asInt_fold "101"
   101
   ghci> asInt_fold "-31337"
   -31337
   ghci> asInt_fold "1798"
   1798
   ```

   ```
   import Data.Char (digitToInt)

   asInt_fold :: String -> Int
   asInt_fold ('-':xs) = 0 - asInt_fold xs
   asInt_fold xs = foldl step 0 xs
     where step a x = 10 * a + digitToInt x
   ```

   Extend your function to handle the following kinds of exceptional conditions by calling `error`.

   ```
   ghci> asInt_fold ""
   0
   ghci> asInt_fold "-"
   0
   ghci> asInt_fold "-3"
   -3
   ghci> asInt_fold "2.7"
   *** Exception: Char.digitToInt: not a digit '.'
   ghci> asInt_fold "314159265358979323846"
   564616105916946374
   ```

   ```
   asInt_fold :: String -> Int
   asInt_fold [] = error "Empty string is not a number"
   asInt_fold "-" = error "'-' is not a number"
   asInt_fold ('-':xs) = 0 - asInt_fold xs
   asInt_fold xs = foldl step 0 xs
     where
       step _ '.' = error "Can't handle numbers with '.'"
       step a x = 10 * a + digitToInt x
   ```

2. The `asInt_fold` function uses `error`, so its callers cannot handle errors.
   Rewrite it to fix this problem.

   ```haskell
   -- file: ch04/ch04.exercises.hs
   type ErrorMessage = String
   asInt_either :: String -> Either ErrorMessage Int
   ```

   ```
   ghci> asInt_either "33"
   Right 33
   ghci> asInt_either "foo"
   Left "non-digit 'o'"
   ```

3. The Prelude function `concat` concatenates a list of lists into a single list, and has the following type.

   ```haskell
   -- file: ch04/ch04.exercises.hs
   concat :: [[a]] -> [a]
   ```

   Write your own definition of `concat` using `foldr`.

4. Write your own definition of the standard `takeWhile` function, first using explicit recursion, then `foldr`.

   ```
   takeWhile' :: (a -> Bool) -> [a] -> [a]
   takeWhile' _ [] = []
   takeWhile' f (x:xs)
     | not $ f x = []
     | otherwise = x:(takeWhile' f xs)

   takeWhile'' :: (a -> Bool) -> [a] -> [a]
   takeWhile'' f xs = foldr step [] xs
     where
       step x a
         | not $ f x = []
         | otherwise = x:a
   ```

5. The `Data.List` module defines a function, `groupBy`, which has the following type.

   ```haskell
   -- file: ch04/ch04.exercises.hs
   groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
   ```

   Use *ghci* to load the `Data.List` module and figure out what `groupBy` does, then write your own implementation using a fold.

6. How many of the following Prelude functions can you rewrite using list folds?

   - any
   - cycle
   - words
   - unlines

   For those functions where you can use either `foldl'` or `foldr`, which is more appropriate in each case?
