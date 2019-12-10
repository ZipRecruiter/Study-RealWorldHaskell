
mydrop _ [] = []
mydrop 0 ls = ls
mydrop n (x:xs) = mydrop (n-1) xs
