
data Tree a = Node { d :: a, left :: Maybe (Tree a), right :: Maybe (Tree a) }
  deriving Show


aTree = Node 3 (Just (Node 1 Nothing (Just (Node 2 Nothing Nothing)))) Nothing

inorder (Node d lt rt) = inorder' lt ++ [d] ++ inorder' rt
  where inorder' = maybe [] inorder

leftmost node = case (left node) of
  Nothing -> d node
  Just lt -> leftmost lt
