import Data.Char (digitToInt)

asInt_fold :: String -> Int
asInt_fold [] = error "Empty string is not a number"
asInt_fold "-" = error "'-' is not a number"
asInt_fold ('-':xs) = 0 - asInt_fold xs
asInt_fold xs = foldl step 0 xs
  where
    step _ '.' = error "Can't handle numbers with '.'"
    step a x = 10 * a + digitToInt x

test = do
  print $ asInt_fold "101"
  print $ asInt_fold "-31337"
  print $ asInt_fold "1798"
  catch (print $ asInt_fold "") printErr
  catch (print $ asInt_fold "-") printErr
  catch (print $ asInt_fold "2.7") printErr
