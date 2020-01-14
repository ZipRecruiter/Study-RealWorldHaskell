
module LexJSON (lexJSON) where

import TokenJSON

import Data.Char (isDigit)

lexJSON :: String -> [JToken]
lexJSON "" = []
lexJSON ('{':xs)  = JTokenObjectStart : lexJSON xs
lexJSON ('}':xs)  = JTokenObjectEnd   : lexJSON xs
lexJSON ('[':xs)  = JTokenArrayStart  : lexJSON xs
lexJSON (']':xs)  = JTokenArrayEnd    : lexJSON xs
lexJSON (':':xs)  = JTokenPairSep     : lexJSON xs
lexJSON (',':xs)  = JTokenListSep     : lexJSON xs

lexJSON xs
  | c `elem` " \t\r\n" = lexJSON $ tail xs
  | (take 4 xs) == "true" =
    JTokenTrue  : (lexJSON $ drop 4 xs)
  | (take 5 xs) == "false" =
    JTokenFalse : (lexJSON $ drop 5 xs)
  | (take 4 xs) == "null" =
    JTokenNull  : (lexJSON $ drop 4 xs)
  | c == '"' =
    JTokenString str : lexJSON afterStr
  | (c == '-' && (isDigit c' || c' == '.' && isDigit c'')) ||
    (isDigit c || c == '.' && isDigit c') =
    JTokenNumber num : lexJSON afterNum
  | otherwise = error $ "Found unexpected char '" ++ c:"' lexing JSON"
  where
    c = head xs
    c' =
      if (length xs > 1)
      then head $ tail xs
      else ' '
    c'' =
      if (length xs > 2)
      then head $ tail $ tail xs
      else ' '
    (str, afterStr) = lexString $ tail xs
    (num, afterNum) = lexNumber xs

lexString :: String -> (String, String)
lexString xs = (init string, rest)
  where
  string = takeString xs
  rest = drop (length string) xs
  takeString :: String -> String
  takeString "" = error "End of stream lexing a string"
  takeString ('\n':_) = error "End of line lexing a string"
  takeString ('\\':'"':ys) = '\\' : '"' : takeString ys
  takeString (y:ys) =
    if y == '"'
    then y:""
    else y: takeString ys

lexNumber :: String -> (String, String)
lexNumber xs = (string, rest)
  where
  string = takeNumber xs
  rest = drop (length string) xs
  takeNumber :: String -> String
  takeNumber ('-':x:xs)
    | x == '.' = '-' : '.': getDecimal xs
    | isDigit x = '-' : getNumber (x:xs)
    | otherwise = error "Invalid char '" ++ x:"' after '-'"
  takeNumber xs = getNumber xs
  getNumber :: String -> String
  getNumber "" = ""
  getNumber ('.':xs) = '.' : getDecimal xs
  getNumber (x:xs)
    | isDigit x = x : getNumber xs
    | otherwise = ""
  getDecimal :: String -> String
  getDecimal = takeWhile isDigit
