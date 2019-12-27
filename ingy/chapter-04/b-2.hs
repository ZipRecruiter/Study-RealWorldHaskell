-- NOT WORKING YET

import Data.Char (digitToInt)

asInt_either :: String -> Either String Int
asInt_either [] = Left "Empty string is not a number"
asInt_either "-" = Left "'-' is not a number"
asInt_either ('-':xs) = 0 - asInt_either xs
asInt_either xs = Right $ foldl step 0 xs
  where
    step _ '.' = Left "Can't handle numbers with '.'"
    step a x = 10 * a + digitToInt x

test = do
  print $ asInt_either "101"
  print $ asInt_either "-31337"
  print $ asInt_either "1798"
  catch (print $ asInt_either "") printErr
  catch (print $ asInt_either "-") printErr
  catch (print $ asInt_either "2.7") printErr
