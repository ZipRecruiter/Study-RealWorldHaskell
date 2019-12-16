data List a = Cons a (List a) | Nil deriving Show

fromList Nil = []
fromList (Cons a as) = a:(fromList as)

foldList f i Nil = i
foldList f i (Cons a as) = f a (foldList f i as)

fromList' = foldList (:) []

instance Foldable List  where
  foldMap _ Nil = mempty
  foldMap f (Cons a rest) = mappend (f a) (foldMap f rest)

fromList'' :: List a -> [a]
fromList'' = foldr (:) []


testList = Cons 1 (Cons 4 (Cons 7 Nil))
