data P = P Float Float
  deriving (Show)

data Direction = L | R | S
  deriving (Show)

turns :: [P] -> [Direction]
turns [a,b,c] = [ turn a b c ]
turns (a:b:c:xs) = (turn a b c):(turns $ b:c:xs)
turns _ = error "Need at least 3 points"

turn :: P -> P -> P -> Direction
turn (P ax ay) (P bx by) (P cx cy)
    | (calc == 0) = S
    | (calc > 0) = L
    | (calc < 0) = R
      where
        calc = v' - v
        v = (by - ay) `atan2` (bx - ax)
        v' = (cy - by) `atan2` (cx - bx)
