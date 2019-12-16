
data Point a = Point { x :: a, y :: a }
               deriving (Eq, Show)

data Direction = Lt | Straight | Rt
               deriving (Eq, Ord, Show)

p_op :: (a -> a -> a) -> Point a -> Point a -> Point a
p_op op (Point x1 y1) (Point x2 y2) = Point (x1 `op` x2) (y1 `op` y2)

-- convert radians to degrees
degrees n = 180 * n / pi

-- direction of vector from a to b
-- dir a b = let angle = atan2 (p_op (-) a b)
dir a b =
  let Point { x = xd, y = yd } = p_op (-) b a
  in degrees $ atan2 yd xd 
    
turn a b c =
  let d1 = dir b c - dir a b
  in
    if d1 == 0 then Straight
    else if d1 > 0 then Lt
         else Rt

turns' (a:b:c:rest) = (turn a b c) : (turns' (b:c:rest))
turns' _ = []

turns (a:b:c:rest) = turns' $ (a:b:c:rest) ++ [a,b]

square1 = [Point (-1.0) 0.0, Point 0.0 (-1.0), Point 1.0 0.0, Point 0.0 1.0]

