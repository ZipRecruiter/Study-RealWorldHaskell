# Chapter 4 exercises

## Working with Lists

1. Write your own “safe” definitions of the standard partial list functions, but make sure that yours never fail. As a hint, you might want to consider using the following types.

[ safeList.hs ]

2. Write a function `splitWith` that acts similarly to `words`, but takes a predicate and a list of any type, and splits its input list on every element for which the predicate returns `False`.

[ splitList.hs ]

3. Using the command framework from the section called “A simple command line framework”, write a program that prints the first word of each line of its input.

[ firstWord.hs ]

4. Write a program that transposes the text in a file. For instance, it should convert `"hello\nworld\n"` to `"hw\neo\nlr\nll\nod\n"`.

[ transpose.hs ]

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

[asInt.hs]

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

[asInt.hs]

3. The Prelude function `concat` concatenates a list of lists into a single list, and has the following type.

```haskell
-- file: ch04/ch04.exercises.hs
concat :: [[a]] -> [a]
```


Write your own definition of `concat` using `foldr`.

[concat.hs]

4. Write your own definition of the standard `takeWhile` function, first using explicit recursion, then `foldr`.

[takeWhile.hs]

5. The `Data.List` module defines a function, `groupBy`, which has the following type.

```haskell
-- file: ch04/ch04.exercises.hs
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
```

Use *ghci* to load the `Data.List` module and figure out what `groupBy` does, then write your own implementation using a fold.

[groupBy.hs]

6. How many of the following Prelude functions can you rewrite using list folds?
  - any
  - cycle
  - words
  - unlines

For those functions where you can use either `foldl'` or `foldr`, which is more appropriate in each case?

[moreFolds.hs]
