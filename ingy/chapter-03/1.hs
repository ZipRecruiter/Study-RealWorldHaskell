length' xs = foldl1 (+) (0:(map (const 1) xs))
