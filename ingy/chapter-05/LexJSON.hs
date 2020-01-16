
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
  | (c `elem` "-.0123456789") =
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
  takeNumber ('-':"") = error "Error lexing number"
  takeNumber ('-':xs) = '-':getNumber xs
  takeNumber xs = getNumber xs

  getNumber :: String -> String
  getNumber xs
    | a == '.' = a : (getDecimal $ tail xs)
    | a `elem` "eE" = a : (getExponent $ tail xs)
    | a == '0' && isDigit b = error "Number can't start with 0"
    | isDigit a = a : (getNumber $ tail xs)
    | otherwise = ""
    where (a:b:_) = xs ++ "  "

  getDecimal :: String -> String
  getDecimal = takeWhile isDigit

  getExponent :: String -> String
  getExponent xs
    | a `elem` "+-" || isDigit a = a : (takeWhile isDigit $ tail xs)
    | otherwise = error "Error parsing exponent of number"
    where (a:_) = xs ++ " "

is1'9 = (`elem` ['1'..'9'])
