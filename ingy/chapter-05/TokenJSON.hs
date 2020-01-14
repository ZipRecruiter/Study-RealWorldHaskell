
module TokenJSON (
  JToken(..),
  tokenString,
  tokenNumber,
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

tokenString :: JToken -> String
tokenString (JTokenString s) = s

tokenNumber :: JToken -> String
tokenNumber (JTokenNumber n) = n
