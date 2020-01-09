module JsunPrint (prJVal) where

import Jsun
import Data.List (intercalate)

renderJVal :: JVal -> String
renderJVal (JStr x)      = show x
renderJVal (JNum x)      = show x
renderJVal (JBool True)  = "true"
renderJVal (JBool False) = "false"
renderJVal JNull         = "null"

renderJVal (JObj x) = "{" ++ pairs x ++ "}"
  where pairs [] = ""
        pairs xs = intercalate ", " $ map renderPair xs
        renderPair (k,v) = show k ++ ": " ++ renderJVal v

renderJVal (JArr x) = "[" ++ values x ++ "]"
  where values [] = ""
        values xs = intercalate ", " $ map renderJVal xs


prJVal :: JVal -> IO()
prJVal x = putStrLn $ renderJVal x
