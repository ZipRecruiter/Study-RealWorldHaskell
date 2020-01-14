module YAJP7 (JVal(..), yajp7_parse) where

import Text.ParserCombinators.ReadP
import Control.Applicative
import Control.Exception

data JVal = JStr String
          | JNum Double
          | JBool Bool
          | JNull
          | JObj [(String, JVal)]
          | JArr [JVal]
          | JErr String
            deriving (Eq, Ord, Show)

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

yajp7_parse :: [Char] -> IO JVal
yajp7_parse x = do
  result <- catchAny (yajp7_parse' x) $ \e -> do
      return $ JErr (show e)
  return result

yajp7_parse' :: [Char] -> IO JVal
yajp7_parse' x = if null $ snd r
                 then return $ fst r
                 else error $
                   "You left some garbage at the end of your string noob: " ++ (snd r)
  where r = parseJSON' x

parseJSON' :: [Char] -> (JVal, String)
parseJSON' []        = error "Expected json, received emptry string."
parseJSON' x@(xs:xt) = let (j, r) = head (pstr n <|> pnum n <|> pbool n <|> parr n <|> pobj n <|> err n)
                       in (j, r)
  where n   = ltrim' x

uniq :: (Eq a) => [(a, b)] -> [(a, b)]
uniq [] = []
uniq (x:xs)
     | null $ fmap (\c -> k == fst c) xs = uniq xs
     | otherwise = [x] ++ uniq xs
  where k = fst x

pobj :: String -> [(JVal, String)]
pobj i
     | (head i') == '{' = if null i''
                          then error "Unexpected end of string"
                          else if head i'' == '}'
                               then [(JObj [], ltrim' $ tail i'')]
                               else prcss $ kvp i'' 
     | otherwise = []
  where
    i' = ltrim' i
    i'' = ltrim' $ tail i'
    prcss c = [(flattened, r)]
            where
              flattened = JObj $ map fst $ uniq $ reverse c
              last7     = ltrim' $ snd $ last c
              r = if '}' /= head last7
                  then error $ "What kind of idiot forgets the object terminator? GOT: " ++ [head last7]
                  else ltrim' $ tail last7
    kvp i = case readP_to_S (satisfy (=='"')) i of
        [] -> []
        [(m, r)] -> f (rstr 0 r) r
      where f x r' = if null rest || head rest /= ':'
                     then error rest -- "Expected colon after key in key value pair."
                     else [((
                         snd x
                       , fst val
                     ), rest')] ++ nxt 
                     where rest  = ltrim' $ drop (fst x) r'
                           val   = parseJSON' $ ltrim' $ tail rest
                           rest' = ltrim' $ snd val
                           rnull = not $ null $rest'
                           nxt   = if rnull && ',' == head rest'
                                   then kvp . ltrim' $ tail rest'
                                   else if rnull && '}' == head rest'
                                        then []
                                        else error $ "What kind of idiot forgets the object terminator?"

parr :: String -> [(JVal, String)]
parr (i:is) 
     | i == '[' = if head is' == ']' then [(JArr [], tail is')] else parrv' is'
     | otherwise = []
  where is' = ltrim' is

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
                        r = ltrim' $ snd v
                        p = head r
                        recurse = if p == ','
                                  then parrv'' (tail r)
                                  else if p /= ']'
                                       then error $ "Like you, all arrays must come to an end.  GOT:" ++ [p]
                                       else []

pstr :: String -> [(JVal, String)]
pstr i = case readP_to_S (satisfy (=='"')) i of
    [] -> []
    [(m, r)] -> f r $ rstr 0 r
  where f r x = [(JStr (snd x), ltrim' $ drop (fst x) r)]

pnum :: String -> [(JVal, String)]
pnum i = case readP_to_S (satisfy (\c -> any (c==) (['0'..'9'] ++ "-."))) i of
    [] -> []
    [(m, r)] -> f r (rnum ([m] ++ r))
  where f r x = [(JNum (snd x), ltrim' $ drop (-1 + fst x) r)]

pbool :: String -> [(JVal, String)]
pbool x
      | take 4 x == "true" = [(JBool True, drop 4 x)]
      | take 5 x == "false" = [(JBool True, drop 5 x)]
      | take 4 x == "null" = [(JNull, drop 4 x)]
      | otherwise = []

err :: String -> [(JVal, String)]
err [] = []
err x  = error $ "Unexpected input:" ++ x

rstr :: Int -> String -> (Int, String)
rstr _ [] = error "Unterminated string."
rstr i (x:x':xs) = if x /= '\\' && x' == '"'
                   then (i + 2, [x])
                   else (i + fst esc + fst nxt, snd esc ++ snd nxt) 
  where esc = if x' == '"' then (2, "\"") else (1, [x])
        nxt = rstr i $ if x' == '"' then xs else x': xs

rnum :: String -> (Int, Double)
rnum [] = error "Expected number."
rnum x = (length rnum'', read rnum'' :: Double)
  where rnum' (x:xs) = [x] ++ if null xs || (\c -> all (c/=) "0123456789.") pxs then [] else rnum' xs
          where pxs = head xs
        rnum'' = rnum' x

ltrim :: [Char] -> (Int, String)
ltrim x = (length $ fst s, snd s)
  where s = break (\y -> all (y/=) "\t\r\n ") x
ltrim' :: [Char] -> String
ltrim' = snd . ltrim
