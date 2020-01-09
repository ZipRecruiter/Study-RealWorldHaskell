module Jsun 
    (
        JVal(..)
      , getStr
      , getInt
      , getDouble
      , getBool
      , getObj
      , getArr
      , isNull
    ) where

data JVal = JStr String
          | JNum Double
          | JBool Bool
          | JNull
          | JObj [(String, JVal)]
          | JArr [JVal]
            deriving (Eq, Ord, Show)

getStr :: JVal -> Maybe String
getStr (JStr s) = Just s
getStr _        = Nothing

getInt (JNum n) = Just (truncate n)
getInt _        = Nothing

getDouble (JNum n) = Just n
getDouble _        = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObj (JObj o) = Just o
getObj _        = Nothing

getArr (JArr a) = Just a
getArr _        = Nothing

isNull v          = v == JNull
