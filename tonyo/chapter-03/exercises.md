
## Exercises
1. Write the converse of `fromList` for the `List` type: a function that takes a `List a` and generates a `[a]`.

```haskell
data List a = Cons a (List a)
            | Nil
              deriving (Show)

toList :: List a -> [a]
toList (Cons x xs) = x : toList xs
toList Nil = []
```

2. Define a tree type that has only one constructor, like our Java example.
   Instead of the `Empty` constructor, use the `Maybe` type to refer to a node's children.

```haskell
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)

-- Node "parent" (Just (Node "child1" Nothing Nothing)) Nothing
```

## Exercises

1. Write a function that computes the number of elements in a list.
   To test it, ensure that it gives the same answers as the standard `length` function.

```haskell
len :: [a] -> Int
len [] = 0
len xs = len' xs 0
         where
           len' (x:xs) c = len' xs (c + 1)
           len' [] c = c
```

2. Add a type signature for your function to your source file.
   To test it, load the source file into ghci again.

3. Write a function that computes the mean of a list, i.e. the sum of all elements in the list divided by its length.
   (You may need to use the `fromIntegral` function to convert the length of the list from an integer into a floating point number.)

```haskell
mean :: Fractional a => [a] -> a
mean [] = 0
mean xs = (sum' xs 0) / (len' xs 0)
         where
           sum' (x:xs) c = sum' xs (c + x)
           sum' [] c = c
           len' (x:xs) c = len' xs (c + 1)
           len' [] c = c
```

4. Turn a list into a palindrome, i.e. it should read the same both backwards and forwards.
   For example, given the list `[1,2,3]`, your function should return `[1,2,3,3,2,1]`.

```haskell
pali :: [a] -> [a]
pali [] = []
pali (xs) = xs ++ (reverse xs)
```

5. Write a function that determines whether its input list is a palindrome.

```haskell
is_pali :: Eq a => [a] -> Bool
is_pali [] = True
is_pali (xs) = case mod (length xs) 2 of
                 0 -> a == b
                      where
                        a = drop (div (length xs) 2) xs
                        b = drop (div (length xs) 2) (reverse xs)
                 _ -> False
```

6. Create a function that sorts a list of lists based on the length of each sublist.
   (You may want to look at the `sortBy` function from the `Data.List` module.)

```haskell
import Data.List

sortByLen :: [[a]] -> [[a]]
sortByLen (xs) = sortBy sortByLen' xs
                 where
                   sortByLen' :: [a] -> [b] -> Ordering
                   sortByLen' xs ys = compare (length xs) (length ys)
```

7. Define a function that joins a list of lists together using a separator value.
   ```
   -- file: ch03/Intersperse.hs
   intersperse :: a -> [[a]] -> [a]
   ```

   The separator should appear between elements of the list, but should not follow the last element.
   Your function should behave as follows.

   ```
   ghci> :load Intersperse
   [1 of 1] Compiling Main             ( Intersperse.hs, interpreted )
   Ok, modules loaded: Main.
   ghci> intersperse ',' []
   ""
   ghci> intersperse ',' ["foo"]
   "foo"
   ghci> intersperse ',' ["foo","bar","baz","quux"]
   "foo,bar,baz,quux"
   ```

```haskell
intersperse :: Char -> [[Char]] -> [Char]
intersperse a [] = []
intersperse a (x:[]) = x
intersperse a (x:xs) = x ++ [a] ++ (intersperse a xs)
```

8. Using the binary `tree` type that we defined earlier in this chapter, write a function that will determine the height of the tree.
   The height is the largest number of hops from the root to an `Empty`.
   For example, the `tree Empty` has height zero; `Node "x" Empty Empty` has height one; `Node "x" Empty (Node "y" Empty Empty)` has height two; and so on.

```haskell
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)

treeHeight :: Maybe (Tree a) -> Int
treeHeight Nothing = 0
treeHeight (Just (Node n lt rt)) = 1 + max (treeHeight lt) (treeHeight rt)

-- test tree1
--        "parent"
--       /        \
--     child1     Nothing
--    /      \
-- child2    Nothing
tree1 = Node "parent" (Just (Node "child1" (Just (Node "child2" Nothing Nothing)) Nothing)) Nothing


-- test tree2 (unbalanced right)
--  "p"
--  |  \
-- Not  1
--      | \
--      N  2
--         | \
--         N  3
tree2 = Node "parent" Nothing (Just $ Node "1" Nothing (Just $ Node "2" Nothing (Just $ Node "3" Nothing Nothing)))
```

9. Consider three two-dimensional points `a`, `b`, and `c`.
   If we look at the angle formed by the line segment from `a` to `b` and the line segment from `b` to `c`, it either turns left, turns right, or forms a straight line.
   Define a `Direction` data type that lets you represent these possibilities.

```haskell
data Direction = Left | Right | Straight
```

10. Write a function that calculates the turn made by three 2D points and returns a `Direction`.

11. Define a function that takes a list of 2D points and computes the direction of each successive triple.
    Given a list of points `[a,b,c,d,e]`, it should begin by computing the turn made by `[a,b,c]`, then the turn made by `[b,c,d]`, then `[c,d,e]`.
    Your function should return a list of `Direction`.

12. Using the code from the preceding three exercises, implement Graham's scan algorithm for the convex hull of a set of 2D points.
    You can find good description of what a [convex hull](http://en.wikipedia.org/wiki/Convex_hull) is, and how the [Graham scan algorithm](http://en.wikipedia.org/wiki/Graham_scan) should work, on Wikipedia.
