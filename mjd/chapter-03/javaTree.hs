
data Tree a = Node { d :: a, left :: Maybe (Tree a), right :: Maybe (Tree a) }
  deriving Show


treeheight t = treeheight' (Just t) where
  treeheight' Nothing = 0
  treeheight' (Just (Node {left=lt, right=rt})) = 1 + max (treeheight' lt) (treeheight' rt)

treerec op zero t = treerec' (Just t) where
  treerec' Nothing = zero
  treerec' (Just (Node { left=lt, right=rt })) = treerec' lt `op` treerec' rt

treeheight2 = treerec (\l r -> 1 + max l r) 0 

aTree = Node 3 (Just (Node 1 Nothing (Just (Node 2 Nothing Nothing)))) Nothing

inorder (Node d lt rt) = inorder' lt ++ [d] ++ inorder' rt
  where inorder' = maybe [] inorder

leftmost node = case (left node) of
  Nothing -> d node
  Just lt -> leftmost lt
