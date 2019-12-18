data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

tree = Node "parent"
   (Node "left child" Empty Empty)
   (Node "right child" Empty Empty)

data Tree' a = Node' a (Maybe (Tree' a)) (Maybe (Tree' a))
   deriving (Show)

tree' = Node' "parent"
   (Just $ Node' "left child" Nothing Nothing)
   (Just $ Node' "right child" Nothing Nothing)
