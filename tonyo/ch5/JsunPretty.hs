module JsunPretty (prJVal) where

import Jsun
import Data.List (intercalate)

renderJVal :: JVal -> Int -> String
renderJVal (JStr x)      _ = show x
renderJVal (JNum x)      _ = show x
renderJVal (JBool True)  _ = "true"
renderJVal (JBool False) _ = "false"
renderJVal JNull         _ = "null"

renderJVal (JObj x) i = replicate i ' ' ++ "{\n" ++ pairs x ++ replicate i ' ' ++ "}"
  where pairs [] = ""
        pairs xs = (intercalate ",\n" $ map (`renderPair` (i+2)) xs) ++ "\n" 
        renderPair (k,v) i = (replicate i ' ') ++ show k ++ ": " ++ renderJVal v i

renderJVal (JArr x) i = "[" ++ values x ++ "]"
  where values [] = ""
        values xs = intercalate ", " $ map (`renderJVal` 0) xs


prJVal :: JVal -> IO()
prJVal x = putStrLn $ renderJVal x 0
