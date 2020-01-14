module ParseJSON (parseJSON) where

import SimpleJSON
import TokenJSON

parseJSON :: [JToken] -> JValue
parseJSON (JTokenObjectStart:xs) = parseJSONObject xs
parseJSON [] = []

parseJSONObject xs = JTrue
