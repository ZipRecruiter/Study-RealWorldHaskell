module YAJP7 (JVal(..), yajp7_fparse, yajp7_parse, yajp7_print) where

import Text.ParserCombinators.ReadP
import Control.Applicative
import Control.Exception
import Data.List (intercalate)
import Numeric (showHex)

data JVal = JStr String
          | JNum Double
          | JBool Bool
          | JNull
          | JObj [(String, JVal)]
          | JArr [JVal]
          | JErr String
            deriving (Eq, Ord, Show)


renderJVal :: JVal -> String
renderJVal (JStr x)      = fixOut $ show x
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

fixOut :: String -> String
fixOut ('\\':'D':'E':'L':xs) = "\\u007F" ++ fixOut xs
fixOut ('\\':x:xs)
    | (\c -> any (c==) "1234567890") x = "\\u" ++ lxs ++ (fixOut $ drop (-1 + length xc) xs)
    | otherwise = "\\" ++ [x] ++ fixOut xs
 where xc = takeWhile (\c -> any (c==) "1234567890abcdefABCDEF") (x:xs)
       lxs = lpad 4 '0' (showHex (read xc :: Int) "") 
fixOut (x:xs) = [x] ++ fixOut xs
fixOut [] = []

yajp7_print :: JVal -> IO()
yajp7_print x = putStrLn $ renderJVal x

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

yajp7_parse :: [Char] -> IO JVal
yajp7_parse x = do
  result <- catchAny (yajp7_parse' x) $ \e -> do
      return $ JErr (show e)
  return result

yajp7_parse' :: [Char] -> IO JVal
yajp7_parse' x = if null $ ltrim $ snd r
                 then return $ fst r
                 else error $
                   "You left some garbage at the end of your string noob: " ++ (snd r)
  where r = parseJSON' x

yajp7_fparse :: [Char] -> JVal
yajp7_fparse [] = error "Expected json, received empty string."
yajp7_fparse x = if null $ ltrim $ snd r
                 then fst r
                 else error $ "You left some garbage at the end of your string noob: " ++ (snd r)
  where r = parseJSON' x

parseJSON' :: [Char] -> (JVal, String)
parseJSON' []        = error "Expected json, received emptry string."
parseJSON' x@(xs:xt) = let (j, r) = head (pstr n <|> pnum n <|> pbn n <|> parr n <|> pobj n <|> err n)
                       in (j, r)
  where n   = ltrim x

uniq :: (Eq a) => [(a, b)] -> [(a, b)]
uniq [] = []
uniq (x:xs)
     | null $ fmap (\c -> if k == fst c then [] else [1]) xs = uniq xs
     | otherwise = [x] ++ uniq xs
  where k = fst x

pobj :: String -> [(JVal, String)]
pobj ('{':i) = 
    if null i' then error "Objects need terminators" else
      if head i' == '}'
      then [(JObj [], tail i')]
      else [(JObj (reverse $ tail k), seq t' (ltrim $ tail r'))]
  where i' = ltrim i
        k  = reverse $ kvp i'
        r' = ltrim $ fst $ head k
        t' = if null r' || head r' /= '}' then error "Objects need terminators" else True
pobj [] = []

kvp :: String -> [(String, JVal)]
kvp ('"':is) = [(key, fst val')] ++ nxt
  where key' = tstr is
        key  = fst key'
        rest = ltrim $ snd key'
        val' = if (head rest) == ':' then parseJSON' $ ltrim $ tail rest
                                     else error "Colon expected in kvp"
        rest' = ltrim $ snd val'
        rhead = head rest'
        nxt   = if null $ snd val' then error "Comma or terminator expected"
                  else if rhead == ',' then kvp $ ltrim $ tail rest'
                    else if rhead == '}' then [(ltrim rest', JNull)]
                      else error "Unexpected input"
kvp [] = [] 
kvp i = error $ "Expected key, got: " ++ show i

parr :: String -> [(JVal, String)]
parr (i:is) 
     | i == '[' = if head is' == ']' then [(JArr [], tail is')] else parrv' is'
     | otherwise = []
  where is' = ltrim is

parrv' :: String -> [(JVal, String)]
parrv' n = prcss $ parrv'' n
  where prcss   c = [(flattened, r)]
                  where
                    flattened = JArr $ map fst c
                    last7     = snd $ last c
                    r = if ']' /= head last7
                        then error $ "Like you, all arrays must come to an end.  GOT:" ++ [head last7]
                        else tail last7
        parrv'' n = [(fst v, r)] ++ recurse
                  where v = parseJSON' n
                        r = ltrim $ snd v
                        p = head r
                        recurse = if p == ','
                                  then parrv'' (tail r)
                                  else if p /= ']'
                                       then error $ "Like you, all arrays must come to an end.  GOT:" ++ [p]
                                       else []

pstr :: String -> [(JVal, String)]
pstr ('"':is) = [(JStr (fst x), ltrim $ snd x)]
  where x = tstr is
pstr _ = []

pnum :: String -> [(JVal, String)]
pnum i'@(i:is)
    | (\c -> any (c==) "1234567890+-Ee.") i = [(JNum (fst x), ltrim $ snd x)]
    | otherwise = []
  where x = tnum i'
pnum _ = [] 

pbn :: String -> [(JVal, String)]
pbn x
    | take 4 x == "true" = [(JBool True, drop 4 x)]
    | take 5 x == "false" = [(JBool False, drop 5 x)]
    | take 4 x == "null" = [(JNull, drop 4 x)]
    | otherwise = []

err :: String -> [(JVal, String)]
err [] = []
err x  = error $ "Unexpected input:" ++ x

tstr :: String -> (String, String)
tstr ('"':xs) = ([],xs)
tstr ('\\':x:xs) = (fst t ++ fst n, snd n)
  where t = case x of
              'u' -> ("\\u" ++ (uni $ take 4 xs), drop 4 xs)
              '"' -> ("\\\"", xs)
              x | (\c -> any (c==) "\\bfnrt/") x -> ("\\"++[x], xs)
              _ -> error "Invalid escape sequence."
        n = tstr $ snd t
        uni x = if (length x) == 4 && (foldr (\c a -> a && any(c==) "1234567890abcdefABCDEF") True x) then x else error "Invalid unicode."
tstr (x:xs) = ([x'] ++ fst n, snd n)
  where x' = if (\c -> any (c==) "\0\r\n\t") x then error "Invalid character in string" else x
        n = tstr xs

tnum :: String -> (Double, String)
tnum [] = error "Expected a number."
tnum x = (read (fst r) :: Double, snd r)
  where tnum' [] = ("", "")
        tnum' i'@(i:is) = case i of
                     i | (\c -> any (c==) "1234567890+-Ee.") i ->
                        ([i] ++ (fst n), snd n)
                     _ -> ("", i') 
          where n = tnum' is
        v (a,b) = if a'' == '0' && (length $ take 2 a') > 1 && ((\c -> all (c/=) "eE.") (head $ reverse $ take 2 a'))
                  then error $ "Not a valid number as far as JSON is concerned, maybe YAML will have you. GOT: " ++ a
                  else (a,b)
          where a'  = if head a == '-' then tail a else a
                a'' = head a'
        r = v $ tnum' x

ltrim :: [Char] -> String
ltrim x = snd s 
  where s = break (\y -> all (y/=) "\t\r\n ") x

lpad :: Int -> Char -> String -> String
lpad i c v = if (length $ take i v) >= i then v else lpad i c ([c] ++ v)

































































































































































































































