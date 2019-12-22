# Chapter 4 exercises

## Working with Lists

1. Write your own “safe” definitions of the standard partial list functions, but make sure that yours never fail. As a hint, you might want to consider using the following types.

```haskell
-- file: ch04/ch04.exercises.hs
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]
```

### solution

```haskell
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
```

2. Write a function `splitWith` that acts similarly to `words`, but takes a predicate and a list of any type, and splits its input list on every element for which the predicate returns `False`.

```haskell
-- file: ch04/ch04.exercises.hs
splitWith :: (a -> Bool) -> [a] -> [[a]]
```

### solution

```haskell
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ []   = []
splitWith f (xs) = let (fore, aft) = break f (dropWhile f xs)
                   in [fore] ++ (splitWith f aft)
```

3. Using the command framework from the section called “A simple command line framework”, write a program that prints the first word of each line of its input.

*y.test*
```
hello xyz world!
line2first some garbage line2last
line3first xjhaewkjr hewkjrh kjewhr kjewahr jkewahr lewkahr jklaweht jklaewht kljwehtjklaweh rjklwahr ljkwehr lkjewhr lkwah line3last

one-word-line
blank line above line-above
```

```haskell
import System.Environment (getArgs)
import Text.Printf

showFirstLast :: [String] -> IO ()
showFirstLast [] = printf "done\n"
showFirstLast ([]:xs) = do
    printf "(empty line)\n"
    showFirstLast xs
showFirstLast (x:xs)  = do
    printf "'%s', '%s'\n" first lastx
    showFirstLast xs
    where
        wx    = words x
        first = head wx
        lastx = last wx

splitShow :: String -> IO ()
splitShow inputFile = do
    fileData <- readFile inputFile
    let linex = lines fileData
    showFirstLast linex

main = do
    args <- getArgs
    case args of
        [input] -> splitShow input
        _       -> printf "error: exactly one argument needed\n"
```

*output*
```
λ /tmp$ runghc x.hs y.test
Loaded package environment from /Users/tonyo/.ghc/x86_64-darwin-8.6.5/environments/default
'hello', 'world!'
'line2first', 'line2last'
'line3first', 'line3last'
(empty line)
'one-word-line', 'one-word-line'
'blank', 'line-above'
done
```
4. Write a program that transposes the text in a file. For instance, it should convert `"hello\nworld\n"` to `"hw\neo\nlr\nll\nod\n"`.

### solution

```haskell
mport System.Environment (getArgs)
import Text.Printf

transpose :: [Char] -> [Char] -> [Char]
transpose (x:xs) (y:ys) = [x, y, '\n'] ++ (transpose xs ys)
transpose _ (y:ys) = [' ', y, '\n'] ++ (transpose [] ys)
transpose (x:xs) _ = [x, ' ', '\n'] ++ (transpose xs [])
transpose _ _ = []

showFirstLast :: [String] -> IO ()
showFirstLast [] = printf "done\n"
showFirstLast (x:[]) = do
    printf "trailing line: %s\n" x
    showFirstLast []
showFirstLast (x:y:xs)  = do
    print tx
    showFirstLast xs
    where
        tx = transpose x y


splitShow :: String -> IO ()
splitShow inputFile = do
    fileData <- readFile inputFile
    let linex = lines fileData
    showFirstLast linex

main = do
    args <- getArgs
    case args of
        [input] -> splitShow input
        _       -> printf "error: exactly one argument needed\n"
```

*output*
```
λ /tmp$ cat y.test
hello
world
λ /tmp$ runghc x.hs y.test
Loaded package environment from /Users/tonyo/.ghc/x86_64-darwin-8.6.5/environments/default
"hw\neo\nlr\nll\nod\n"
done
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
```haskell
import Data.Char (digitToInt)

asInt_fold :: String -> Int
asInt_fold x = (*) (if head x == '-' then -1 else 1) (fst $ foldr toInt' (0, 0) (if head x == '-' then drop 1 x else x))
               where
                  toInt' :: Char -> (Int, Int) -> (Int, Int)
                  toInt' x (c, p) = (c + (digitToInt(x) * (10 ^ p)), p + 1)
```

2. The `asInt_fold` function uses `error`, so its callers cannot handle errors. Rewrite it to fix this problem.

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

*solution*

```haskell
import Data.Char (digitToInt)

asInt_fold :: String -> Int
asInt_fold x = (*) (if head x == '-' then -1 else 1) (fst $ foldr toInt' (0, 0) (if head x == '-' then drop 1 x else x))
               where
                  toInt' :: Char -> (Int, Int) -> (Int, Int)
                  toInt' x (c, p) = (c + (digitToInt(x) * (10 ^ p)), p + 1)

isInt :: String -> Bool
isInt [] = True
isInt (x:xs) = compare x '0' >= EQ && compare x '9' <= EQ && isInt xs

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either x | (isInt $ if head x == '-' then drop 1 x else x) = Right $ asInt_fold x
               | otherwise = Left  $ error "Input not a valid integer"
```

3. The Prelude function `concat` concatenates a list of lists into a single list, and has the following type.

```haskell
-- file: ch04/ch04.exercises.hs
concat :: [[a]] -> [a]
```

Write your own definition of `concat` using `foldr`.

```haskell
concatT :: [[a]] -> [a]
concatT xs = foldr concatT' [] xs
             where
               concatT' :: [a] -> [a] -> [a]
               concatT' a x = a ++ x
```

4. Write your own definition of the standard `takeWhile` function, first using explicit recursion, then `foldr`.

```haskell
takeWhileR :: (a -> Bool) -> [a] -> [a]
takeWhileR _ [] = []
takeWhileR f (x:xs) = (if f x then [x] ++ takeWhileR f xs else [])

takeWhileF :: Foldable t => (a -> Bool) -> t a -> [a]
takeWhileF f x = foldr tWF' [] x
                 where
                  tWF' x c = if f x then x : c else []
```

5. The `Data.List` module defines a function, `groupBy`, which has the following type.

```haskell
-- file: ch04/ch04.exercises.hs
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
```

Use *ghci* to load the `Data.List` module and figure out what `groupBy` does, then write your own implementation using a fold.

*solution*
```haskell
import Data.List (groupBy)

groupByF :: (a -> a -> Bool) -> [a] -> [[a]]
groupByF f (x:xs) = scnd' $ foldl groupByF' ([], [x], x) xs
               where
                 groupByF' (a, b, c) x = if f c x then (a, b ++ [x], x) else (a ++ [b], [x], x)
                 scnd' (a,b,_) = a ++ [b]

testgb :: Eq a => (a -> a -> Bool) -> [a] -> (Bool, [[a]], [[a]])
testgb f x = (folded == std, folded, std) 
             where
               folded = groupByF f x
               std    = groupBy f x
```

6. How many of the following Prelude functions can you rewrite using list folds?
  - any
  - cycle
  - words
  - unlines

*solution*
```
all of them.
```

For those functions where you can use either `foldl'` or `foldr`, which is more appropriate in each case?


