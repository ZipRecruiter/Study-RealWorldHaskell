-- 1. Enter the following expressions into ghci. What are their types?

-- 5 + 8
-- Num a => a

-- 3 * 5 + 8
-- Num a => a

-- 2 + 4
-- Num a => a

-- (+) 2 4
-- Num a => a

-- sqrt 16
-- Floating a => a

-- succ 6
-- (Enum a, Num a) => a

-- succ 7
-- (Enum a, Num a) => a

-- pred 9
-- (Enum a, Num a) => a

-- pred 8
-- (Enum a, Num a) => a

-- sin (pi / 2)
-- Floating a => a

-- truncate pi
-- Integral b => b

-- round 3.5
-- Integral b => b

-- round 3.4
-- Integral b => b

-- floor 3.7
-- Integral b => b

-- ceiling 3.3
-- Integral b => b

-- 2. Define a variable, such as let x = 1, then type :show bindings. What do you see?

-- let answer = 42
-- :show bindings
-- answer :: Num p => p = _
