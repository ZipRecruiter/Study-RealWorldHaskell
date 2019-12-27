import Data.List (groupBy)

groupByF :: (a -> a -> Bool) -> [a] -> [[a]]
groupByF f (x:xs) = scnd' $ foldl groupByF' ([], [x], x) xs
               where
                 groupByF' (a, b, c) x = if f c x then (a, b ++ [x], x) else (a ++ [b], [x], x)
                 scnd' (a,b,_) = a ++ [b]
