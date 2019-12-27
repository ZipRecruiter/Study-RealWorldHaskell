anyF :: Foldable t => (a -> Bool) -> t a -> Bool
anyF f xs = foldr (\x c -> f x || c) False xs

cycleF :: [a] -> [a]
cycleF xs = foldr (\_ c -> xs ++ c) [] [1..]

wordsF :: Foldable t => t Char -> [[Char]]
wordsF x = [snd c] ++ fst c
           where
             ws = [' ', '\t', '\n']
             c  = foldr
                  (\x (ca, c1) -> if not (null c1) && any (==x) ws
                                  then ([c1] ++ ca, [])
                                  else (ca, if null c1 && any (==x) ws
                                            then []
                                            else [x] ++ c1))
                  ([], [])
                  x

unlinesF :: Foldable t => t [Char] -> [Char]
unlinesF x = foldr (\x c -> x ++ ['\n'] ++ c) [] x
