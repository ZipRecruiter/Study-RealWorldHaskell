import Data.List

sortByListLength :: Foldable t => [t a] -> [t a]
sortByListLength = sortBy (\a b -> (length a) `compare` (length b))
