
import Data.Char (digitToInt, isDigit)
import Control.Monad (foldM, liftM, liftM2)
type IntParseResult = Either String Int

failure = Left
result =  Right

asInt_0         :: String -> Int
asUnsignedInt_0 :: String -> Int

asInt_1         :: String -> IntParseResult
asUnsignedInt_1 :: String -> IntParseResult

safeDigitToInt :: Char -> IntParseResult
safeDigitToInt x = if isDigit x then result $ digitToInt x else failure $ "Non-digit character '" ++ [x] ++ "'"

asUnsignedInt_0 [] = error "Empty digit sequence"
asUnsignedInt_0 digits = foldl (\a b -> a * 10 + digitToInt b) 0 digits

-- Here I use the monad structure to propagate the error behavior of
-- Either through to the result when there is a bad digit.
-- But using a monad is incompatible with left-to-right processing.
asUnsignedInt_1 [] = failure "Empty digit sequence"
asUnsignedInt_1 [d] = safeDigitToInt d
asUnsignedInt_1 (d:ds) = do
  d_  <- safeDigitToInt d
  ds_ <- asUnsignedInt_1 ds
  return $ ds_ * 10 + d_

-- no, I guess it's not incompatible.  Why doesn't this need the reverse?
asUnsignedInt_2 ""     = failure "Empty digit list"
asUnsignedInt_2 digits = foldM (\b a -> liftM (+ (b * 10)) (safeDigitToInt a)) 0 digits

asInt_0 ('+' : digits) =          asUnsignedInt_0 digits
asInt_0 ('-' : digits) = negate $ asUnsignedInt_0 digits
asInt_0        digits =           asUnsignedInt_0 digits

asInt_1 ('+' : digits) =               asUnsignedInt_1 digits
asInt_1 ('-' : digits) = fmap negate $ asUnsignedInt_1 digits
asInt_1        digits  =               asUnsignedInt_1 digits

asInt_2 ('+' : digits) =              asUnsignedInt_2 digits
asInt_2 ('-' : digits) = fmap negate (asUnsignedInt_2 digits)
asInt_2        digits  =              asUnsignedInt_2 digits

