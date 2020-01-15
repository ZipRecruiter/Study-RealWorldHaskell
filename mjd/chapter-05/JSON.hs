

import Parser
import SimpleJSON

digits = token ['0' .. '9']

sign :: Num a => Parser a
sign = fmap signFactor $ optional $ charclass "+-"
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


  

