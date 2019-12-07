lastButOne xs
  | len == 0 =
    error "List is empty"
  | len == 1 =
    error "List only has 1 item"
  | otherwise =
    head $ drop (len - 2) xs
  where
    len = length xs
