import Data.List

cmpLength :: (Foldable f) => f a -> f a -> Ordering
cmpLength a b = (length a) `compare` (length b)

sortByLength :: Foldable f => [f a] -> [f a]
sortByLength = sortBy cmpLength

        
