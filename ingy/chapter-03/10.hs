data P = P Float Float
  deriving (Show)

data Direction = L | R | S
  deriving (Show)

turn :: P -> P -> P -> Direction
turn (P ax ay) (P bx by) (P cx cy)
    | (calc == 0) = S
    | (calc > 0) = L
    | (calc < 0) = R
      where
        calc = v' - v
        v = (by - ay) `atan2` (bx - ax)
        v' = (cy - by) `atan2` (cx - bx)
