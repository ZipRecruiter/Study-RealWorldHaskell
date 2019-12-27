import Data.Char (digitToInt)

asInt_fold :: String -> Int
asInt_fold x = (*) (if head x == '-' then -1 else 1) (fst $ foldr toInt' (0, 0) (if head x == '-' then drop 1 x else x))
               where
                  toInt' :: Char -> (Int, Int) -> (Int, Int)
                  toInt' x (c, p) = (c + (digitToInt(x) * (10 ^ p)), p + 1)

isInt :: String -> Bool
isInt [] = True
isInt (x:xs) = compare x '0' >= EQ && compare x '9' <= EQ && isInt xs

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either x | (isInt $ if head x == '-' then drop 1 x else x) = Right $ asInt_fold x
               | otherwise = Left  $ error "Input not a valid integer"
