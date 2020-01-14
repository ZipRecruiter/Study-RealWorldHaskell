
module TokenJSON (
  JToken(..),
  getString,
  getNumber,
) where

data JToken = JTokenObjectStart
            | JTokenObjectEnd
            | JTokenArrayStart
            | JTokenArrayEnd
            | JTokenPairSep
            | JTokenListSep
            | JTokenString String
            | JTokenNumber String
            | JTokenTrue
            | JTokenFalse
            | JTokenNull
              deriving (Eq, Show)

getString :: JToken -> String
getString (JTokenString s) = s

getNumber :: JToken -> String
getNumber (JTokenNumber n) = n
