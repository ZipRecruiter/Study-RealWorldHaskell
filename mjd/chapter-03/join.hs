
intersperse sep [] = ""
intersperse sep (a:rest) 
  | rest == []    =  a
  | otherwise     =  a ++ sep ++ intersperse sep rest
