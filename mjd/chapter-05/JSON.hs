
module JSON (json_value,
             is_valid_json
            ) where

import Parser
import SimpleJSON
import Data.Maybe (isJust)

digits = token ['0' .. '9']
optionalDigits = tokenE ['0' .. '9']

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
jintP :: Parser JValue
jintP = intP <|> JNumber

doubleP :: (Read a, Floating a) => Parser a
doubleP = do
  sf <- sign
  digit_and_point <- (seqStrings [optionalDigits, lit ".", optionalDigits ] ) `sideCondition` (/= ".")
  exponent <- (intP `after` charclass "Ee") `orElse` pure 0
  return $ sf * (read $ '0' : digit_and_point ++ "0") * (10 ** exponent)

jdoubleP = doubleP <|> JNumber

dquote = charser '\"'
squote = charser '\''
backslash = charser '\\'

-- assoc is an assoc list
-- if the key is found in the assoc list, the corresponding value is returned
-- otherwise the key is returned unchanged
maybe_translate :: Eq a => [(a, a)] -> a -> a
maybe_translate assoc key = maybe key id (lookup key assoc)

-- mapping from escape symbols to the characters they represent
escs = ("nt'\"\\", "\n\t'\"\\")
esc_lookup = maybe_translate $ zipWith (,) (fst escs) (snd escs)
esc_translate s = map esc_lookup s

non_special_char = charclass $ ['A' .. 'Z'] ++ [ 'a' .. 'z' ] ++ [ '0' .. '9' ]
escaped_special_char = (charclass (fst escs) `after` backslash) <|> esc_lookup

stringP :: Parser String
stringP = enclosed_by dquote contents
  where contents = star (non_special_char `orElse` escaped_special_char)

jstringP = stringP <|> JString

valueP :: Parser JValue
valueP = alternatives [ jstringP, jdoubleP, objectP, boolP, arrayP, nullP, jintP ]

json_value :: String -> Maybe JValue
json_value s = fmap snd $ run (valueP `before` eof) s

is_valid_json :: String -> Bool
is_valid_json = isJust . json_value

keyPairP :: Parser (String, JValue)
keyPairP = seqp3 (\k _ v -> (k, v))  stringP (charser ':') valueP

objectP :: Parser JValue
objectP = between (charser '{')
          (delimitedList (charser ',') keyPairP)
          (charser '}')       <|> JObject

arrayP :: Parser JValue
arrayP = between (charser '[')
          (delimitedList (charser ',') valueP)
          (charser ']')       <|> JArray

boolP = (lit "true" <|> const (JBool True))
        `orElse`
        (lit "false" <|> const (JBool False))

nullP = lit "null" <|> const JNull

