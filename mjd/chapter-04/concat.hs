
myConcat :: Foldable t => t [a] -> [a]
myConcat ls = foldr (++) [] ls
