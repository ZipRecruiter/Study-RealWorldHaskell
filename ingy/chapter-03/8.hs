data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

tree = Node "parent"
   (Node "left child" Empty Empty)
   (Node "right child" Empty Empty)

binaryHeight :: (Tree a) -> Int
binaryHeight t = maximum $ treePaths t

treePaths (Node n Empty _) = [n]
treePaths (Node n _ Empty) = [n]
treePaths (Node n l r) = 1:(treePaths
treePaths (Node n l r) = 1:(treePaths
