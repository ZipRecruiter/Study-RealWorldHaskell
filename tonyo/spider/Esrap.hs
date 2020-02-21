module Esrap (Tag, Attr, tagAttr, attrKey, attrVal, tagFilter, tokenizeHTML, attrFilter, getAttr1) where

data Attr = Attr String String
            deriving (Show)

-- tag name, attributes, children, cdata
data Tag = Tag String [Attr]
           deriving (Show)

attrKey :: Attr -> String
attrKey (Attr x _) = x

attrVal :: Attr -> String
attrVal (Attr _ x) = x

tokenizeHTML :: String -> [Tag]
tokenizeHTML p = index p

tagFilter :: [Tag] -> String -> [Tag]
tagFilter (x:xs) f
  | tagName x == f = [x] ++ tagFilter xs f
  | otherwise = [] ++ tagFilter xs f
tagFilter [] _ = []

attrFilter :: [Tag] -> String -> [Tag]
attrFilter (x:xs) t
    | null hasAttr = [] ++ attrFilter xs t
    | otherwise = [x] ++ attrFilter xs t
  where hasAttr = foldr (\c f -> if attrKey c == t then [c] else f) [] (tagAttr x)
attrFilter [] _ = []

getAttr1 :: Tag -> String -> String
getAttr1 (Tag _ a) key = foldr (\c f -> if attrKey c == key then attrVal c else f) "" a

tagName :: Tag -> String
tagName (Tag n _) = n

tagAttr :: Tag -> [Attr]
tagAttr (Tag _ n) = n

index :: String -> [Tag]
index (x:xs) = case x of
  '<' -> [Tag tagName (fst tagAttr)] ++ (index $ snd tagAttr)
  _   -> [] ++ index xs
  where tagName = takeWhile (\c -> c /= ' ' && c /= '>') xs
        tagRem  = drop (length tagName) xs
        tagAttr = indexAttributes tagRem
index _ = []

indexAttributes :: String -> ([Attr], String)
indexAttributes (x:[]) = ([], [])
indexAttributes (x:xs) = case x of
  ' ' -> indexAttributes xs
  '/' | head xs == '>' -> ([], tail xs)
  '>' -> ([], xs)
  _   -> ([Attr (fst key) (fst val)] ++ (fst rest), snd rest)
  where key  = parseKey (x:xs)
        val  = if head (snd key) == '=' then parseVal $ tail $ snd key else ("", snd key)
        rest = indexAttributes $ snd val


parseKey :: String -> (String, String)
parseKey x'@('=':xs) = ("", x')
parseKey x'@(' ':xs) = ("", x');
parseKey x'@('>':xs) = ("", x');
parseKey (x:xs) = ([x] ++ fst y, snd y) where y = parseKey xs
parseKey _ = error "could not find end of key"

parseVal :: String -> (String, String)
parseVal x'@(x:xs) = (s, s')
  where cl  = (\c -> any (c==) "'\"") x
        tw' = if cl
              then (\c -> c /= x)
              else (\c -> all (c/=) " />")
        st = if cl then xs else x'
        s  = takeWhile tw' st
        s' = drop ((if cl then 1 else 0) + length s) st
