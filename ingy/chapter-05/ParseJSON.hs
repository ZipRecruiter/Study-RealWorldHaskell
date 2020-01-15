module ParseJSON (parseJSON) where

import TokenJSON
import SimpleJSON

parseJSON :: [JToken] -> JValue
parseJSON ts
  | null ts' = value
  | otherwise = error "Found tokens after parsing a JSON value"
  where (value, ts') = parseJSONValue ts

parseJSONValue :: [JToken] -> (JValue, [JToken])
parseJSONValue [] =
  error "End of tokens before parse finished"
parseJSONValue (JTokenArrayStart:ts) = parseJSONArray ts
parseJSONValue (JTokenObjectStart:ts) = parseJSONObject ts
parseJSONValue ((JTokenString s):ts) = (JString s, ts)
parseJSONValue ((JTokenNumber n):ts) = ((JNumber (read n :: Double)), ts)
parseJSONValue (JTokenTrue:ts) = (JBool True, ts)
parseJSONValue (JTokenFalse:ts) = (JBool False, ts)
parseJSONValue (JTokenNull:ts) = (JNull, ts)
parseJSONValue _ = error "Unexpected token parsing JSON value"

-- Array functions:
parseJSONArray :: [JToken] -> (JValue, [JToken])
parseJSONArray ts = (JArray values, ts')
  where (values, ts') = parseJSONList ts

parseJSONList :: [JToken] -> ([JValue], [JToken])
parseJSONList (JTokenArrayEnd:ts) = ([], ts)
parseJSONList ts = (value:sep ++ values, ts')
  where
  (sep, ts''') = checkSep JTokenArrayEnd ts''
  (value, ts'') = parseJSONValue ts
  (values, ts') = parseJSONList ts'''

checkSep :: JToken -> [JToken] -> ([JValue], [JToken])
checkSep end (t:ts)
  | t == JTokenListSep =
    if length ts > 0 && head ts == end
    then error "Trailing comma not allowed in array"
    else ([], ts)
  | t == end = ([], t:ts)
  | otherwise = error "Expecting ',' or ']' parsing array"

-- Object functions:
parseJSONObject :: [JToken] -> (JValue, [JToken])
parseJSONObject ts = (JObject pairs, ts')
  where (pairs, ts') = parseJSONPairs ts

parseJSONPairs :: [JToken] -> ([(String, JValue)], [JToken])
parseJSONPairs (JTokenObjectEnd:ts) =
  ([], ts)
parseJSONPairs ((JTokenString s):JTokenPairSep:ts) =
  makePairs s ts
parseJSONPairs (JTokenListSep:(JTokenString s):JTokenPairSep:ts) =
  makePairs s ts
parseJSONPairs (JTokenListSep:JTokenObjectEnd:_) =
  error "Trailing comma not allowed in JSON object"
parseJSONPairs _ =
  error "Invalid or missing token while parsing object"

makePairs :: String -> [JToken] -> ([(String, JValue)], [JToken])
makePairs s ts = (pair:pairs, ts')
  where
  pair = (s, value)
  (value, ts'') = parseJSONValue ts
  (pairs, ts') = parseJSONPairs ts''
