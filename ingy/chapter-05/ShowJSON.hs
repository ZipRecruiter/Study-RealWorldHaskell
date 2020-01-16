module ShowJSON where

import SimpleJSON

import Data.List (intercalate)

showJSON :: JValue -> String

showJSON (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v) = show k ++ ": " ++ showJSON v

showJSON (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map showJSON vs)

showJSON (JNumber n) =
  if n - fromInteger (floor n) <= 0.00000000000001
  then show $ floor n
  else show n

showJSON (JString s)   = show s
showJSON (JBool True)  = "true"
showJSON (JBool False) = "false"
showJSON JNull         = "null"
