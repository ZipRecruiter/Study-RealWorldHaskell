
mean :: (Fractional a, Integral a) => [a] -> a

mean [] = error "EMPTY"
mean ls = (sum ls) / ((length ls))

mean' ls = mean'_ 0 0 ls
  where mean'_ s ln [] = s / ln
        mean'_ s ln (a:as) = mean'_ (s+a) (ln+1) as
        
