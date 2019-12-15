intersperse' sep list = init $ foldl (\a b -> a ++ b ++ sep) "" list
