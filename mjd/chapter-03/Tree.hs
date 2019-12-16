
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height Empty = 0
height (Node _ lt rt) = 1 + max (height lt) (height rt)

longestPath Empty = []
longestPath (Node d lt rt) = d : longerOf (longestPath lt) (longestPath rt)
   where longerOf a b = case compare (length a) (length b) of
           LT -> b
           _  -> a

aTree = Node 1 (Node 2 (Node 3 Empty Empty) Empty) (Node 4 Empty (Node 5 Empty (Node 6 Empty Empty)))
