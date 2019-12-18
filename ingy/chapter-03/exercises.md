
## Exercises
1. Write the converse of `fromList` for the `List` type: a function that takes a `List a` and generates a `[a]`.
   ```
   toList (Cons x xs) = x:(toList xs)
   toList Nil = []
   ```

2. Define a tree type that has only one constructor, like our Java example.
   Instead of the `Empty` constructor, use the `Maybe` type to refer to a node's children.
   ```
   data Tree' a = Node' a (Maybe (Tree' a)) (Maybe (Tree' a))
      deriving (Show)

   tree' = Node' "parent"
      (Just $ Node' "left child" Nothing Nothing)
      (Just $ Node' "right child" Nothing Nothing)
   ```


## Exercises

1. Write a function that computes the number of elements in a list.
   To test it, ensure that it gives the same answers as the standard `length` function.
   ```
   length' xs = foldl1 (+) (0:(map (const 1) xs))
   ```

2. Add a type signature for your function to your source file.
   To test it, load the source file into ghci again.
   ```
   length' :: [a] -> Int
   length' xs = foldl1 (+) (0:(map (const 1) xs))
   ```

3. Write a function that computes the mean of a list, i.e. the sum of all elements in the list divided by its length.
   (You may need to use the `fromIntegral` function to convert the length of the list from an integer into a floating point number.)
   ```
   mean xs = (fromIntegral $ sum xs) / (fromIntegral $ length xs)
   ```

4. Turn a list into a palindrome, i.e. it should read the same both backwards and forwards.
   For example, given the list `[1,2,3]`, your function should return `[1,2,3,3,2,1]`.
   ```
   toPalindrome xs = xs ++ reverse xs
   ```

5. Write a function that determines whether its input list is a palindrome.
   ```
   isPalindrome xs = reverse xs == xs
   ```

6. Create a function that sorts a list of lists based on the length of each sublist.
   (You may want to look at the `sortBy` function from the `Data.List` module.)
   ```
   import Data.List

   sortByListLength :: Foldable t => [t a] -> [t a]
   sortByListLength = sortBy (\a b -> (length a) `compare` (length b))
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

   Answer:
   ```
   intersperse' sep list = init $ foldl (\a b -> a ++ b ++ sep) "" list
   ```

8. Using the binary `tree` type that we defined earlier in this chapter, write a function that will determine the height of the tree.
   The height is the largest number of hops from the root to an `Empty`.
   For example, the `tree Empty` has height zero; `Node "x" Empty Empty` has height one; `Node "x" Empty (Node "y" Empty Empty)` has height two; and so on.

9. Consider three two-dimensional points `a`, `b`, and `c`.
   If we look at the angle formed by the line segment from `a` to `b` and the line segment from `b` to `c`, it either turns left, turns right, or forms a straight line.
   Define a `Direction` data type that lets you represent these possibilities.
   ```
   data P = P Int Int
     deriving (Show)

   data Direction = L | R | S
     deriving (Show)
   ```

10. Write a function that calculates the turn made by three 2D points and returns a `Direction`.
    ```
    turn (P ax ay) (P bx by) (P cx cy)
      | (calc == 0) = S
      | (calc > 0) = L
      | (calc < 0) = R
        where
          calc = v' - v
          v = (by - ay) `atan2` (bx - ax)
          v' = (cy - by) `atan2` (cx - bx)
    ```

11. Define a function that takes a list of 2D points and computes the direction of each successive triple.
    Given a list of points `[a,b,c,d,e]`, it should begin by computing the turn made by `[a,b,c]`, then the turn made by `[b,c,d]`, then `[c,d,e]`.
    Your function should return a list of `Direction`.
    ```
    turns :: [P] -> [Direction]
    turns [a,b,c] = [ turn a b c ]
    turns (a:b:c:xs) = (turn a b c):(turns $ b:c:xs)
    turns _ = error "Need at least 3 points"
    ```

12. Using the code from the preceding three exercises, implement Graham's scan algorithm for the convex hull of a set of 2D points.
    You can find good description of what a [convex hull](http://en.wikipedia.org/wiki/Convex_hull) is, and how the [Graham scan algorithm](http://en.wikipedia.org/wiki/Graham_scan) should work, on Wikipedia.
