

import Parser
import SimpleJSON

digit = token ['0' .. '9']
-- digits = fmap (foldr (++) []) (star digit)
digits = plus digit

sign :: Num a => Parser a
sign = fmap signFactor $ optional $ token "+-"
       where signFactor (Just '-') = -1
             signFactor  _         = 1

-- read a (possibly-signed) integer
intP :: (Read a, Num a) => Parser a
intP = do
  sf <- sign
  digit_string <- digits
  return $ sf * (read digit_string)

-- only handles integral numbers for now
doubleP :: Parser JValue
doubleP = fmap (JNumber . fromIntegral) intP


  

