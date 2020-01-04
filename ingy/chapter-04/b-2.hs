import Data.Char (digitToInt)
import Data.Either (isLeft)
import Data.Either.Utils (fromRight)

asInt_either :: String -> Either String Int
asInt_either [] = Left "Empty string is not a number"
asInt_either "-" = Left "'-' is not a number"
asInt_either ('-':xs)
  | isLeft result = result
  | otherwise = Right $ 0 - (fromRight result)
  where
    result = asInt_either xs
asInt_either xs
  | '.' `elem` xs = Left "Can't handle numbers with '.'"
  | otherwise = Right $ foldl step 0 xs
  where
    step a x = 10 * a + digitToInt x

test = do
  print $ asInt_either ""
  print $ asInt_either "-"
  print $ asInt_either "101"
  print $ asInt_either "-31337"
  print $ asInt_either "-3.1337"
  print $ asInt_either "1798"
  print $ asInt_either "2.7"
