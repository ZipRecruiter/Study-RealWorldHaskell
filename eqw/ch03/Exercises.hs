-- Solutions to Chapter 3 in _Real World Haskell_.
-- http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html
module ChapterThree where

import qualified Data.List as List (intersperse, sortBy)
import Data.Set (Set)

-- 1. Write a function that computes the number of elements in a list. To test it,
--    ensure that it gives the same answers as the standard `length` function.
--
-- 2. Add a type signature for your function to your source file.
--    To test it, load the source file into ghci again.
listLength :: [a] -> Int
listLength l = foldr (\_ acc -> acc + 1) 0 l
-- (For context, `length`'s type is the more general `Foldable t => t a -> Int`.)

-- 3. Write a function that computes the mean of a list, i.e. the sum of all
--    elements in the list divided by its length. (You may need to use the
--    fromIntegral function to convert the length of the list from an integer
--    into a floating point number.)
mean :: Fractional a => [a] -> a
mean l = (sum l) / (fromIntegral $ listLength l)

-- 4. Turn a list into a palindrome, i.e. it should read the same both
--    backwards and forwards. For example, given the list `[1,2,3]`,
--    your function should return `[1,2,3,3,2,1]`.
listToPalindrome :: [a] -> [a]
listToPalindrome l = l ++ reverse l

-- 5. Write a function that determines whether its input list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l

-- 6. Create a function that sorts a list of lists based on the length of each sublist.
--    (You may want to look at the `sortBy` function from the `Data.List` module.)
sortListOfListsByLength :: [[a]] -> [[a]]
sortListOfListsByLength lists = List.sortBy (\x y -> compare (listLength x) (listLength y)) lists

-- 7. Define a function that joins a list of lists together using a separator value.
-- intersperse :: a -> [[a]] -> [a]

-- 8. Using the binary tree type that we defined earlier in this chapter, write a
--    function that will determine the height of the tree. The height is the largest
--    number of hops from the root to an `Empty`. For example, the tree `Empty` has
--    height zero; `Node "x" Empty Empty` has height one;
--    `Node "x" Empty (Node "y" Empty Empty)` has height two; and so on.
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving Show

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ leftChild rightChild) = (max (treeHeight leftChild) (treeHeight rightChild)) + 1

-- 9. Consider three two-dimensional points _a_, _b_, and _c_. If we look at the
--    angle formed by the line segment from _a_ to _b_ and the line segment from
--    _b_ to _c_, it either turns left, turns right, or forms a straight line.
--    Define a `Direction` data type that lets you represent these possibilities.
data Direction = Left | Right | Straight deriving Show

-- 10. Write a function that calculates the turn made by three 2D points and
--     returns a `Direction`.
type Point = (Int, Int)

-- calculateTurn :: Point -> Point -> Point -> Direction
-- TK

-- 11. Define a function that takes a list of 2D points and computes the direction
--     of each successive triple. Given a list of points `[a,b,c,d,e]`, it should
--     begin by computing the turn made by `[a,b,c]`, then the turn made by
--     `[b,c,d]`, then `[c,d,e]`. Your function should return a list of `Direction`.
-- getDirections :: [Point] -> [Direction]
-- TK

-- 12. Using the code from the preceding three exercises, implement Graham's scan
--     algorithm for the convex hull of a set of 2D points.

-- Takes a set of Points and returns a list of
-- ordered Points describing the convex hull.
-- See: https://en.wikipedia.org/wiki/Graham_scan
-- grahamScan :: Set Point -> [Point]
-- TK
