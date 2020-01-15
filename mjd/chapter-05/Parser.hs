module Parser
    (
      Parser
    , andThen
    , orElse, alternatives
    , lit, charser, token, tokenE, charclass, ws
    , fails
    , concWith
    , pseq
    , optional
    , assert
    , def
    , star, plus
    , eof
    , before, after
    , app -- Not really
    )

where

-- type of parsers that yield a value of type a

data Parser a = P (String -> Maybe (String, a))

app (P f) s = f s

instance Functor Parser where
  fmap f (P p) = P $ \s -> (fmap . fmap) f (p s)

instance Applicative Parser where
  pure a = P $ \s -> Just (s, a) -- return value a and consume no input
  pf <*> pa = do
    f <- pf
    a <- pa
    return $ f a

instance Monad Parser where
  return a = pure a
  p1 >>= f   = P $ \s1 ->
    case app p1 s1 of
      Nothing -> Nothing
      Just (s2, v1) -> app (f v1) s2

-- pa `andThen` pb = pa >>= (\x -> fmap ((,) x) pb)

andThen :: Parser a -> Parser b -> Parser (a, b)
pa `andThen` pb = do
  v1 <- pa
  v2 <- pb
  return (v1, v2)

pa `orElse` pb = P $
  \s -> case app pa s of Nothing -> app pb s
                         res@(Just _) -> res

alternatives ls = foldr orElse fails ls

charser c = P $ \s -> if s == "" || head s /= c then Nothing
                      else return (tail s, c)
-- charser c = lit [c]

-- charclass "blah" is a Parser Char that matches any of a, b, h, l.
charclass s = alternatives $ map charser s

-- token "blah" is a Parser String that matches as many
-- a, b, h, l characters as possible and then stops
-- empty token allowed
tokenE = star . charclass
-- empty token not allowed
token = plus . charclass

_ `startsWith` "" = True
"" `startsWith` _  = False
(c:cs) `startsWith` (d:ds) = c == d && cs `startsWith` ds
lit :: String -> Parser String
lit x = P $ \a -> if a `startsWith` x then return (drop (length x) a, x)
                  else Nothing
                       
fails = P $ const Nothing

concWith f p1 p2 = do
  v1 <- p1
  v2 <- p2
  return $ f v1 v2

pseq :: [ Parser a ] -> Parser [a]
pseq ps = foldr (concWith (:)) (pure []) ps

optional :: Parser a -> Parser (Maybe a)
optional p = (fmap Just p) `orElse` pure Nothing

-- read no input, succeed if and only if f returns true for the upcoming input
assert :: (String -> Bool) -> Parser ()                  
assert f = P $ \s -> if f s then return (s, ()) else Nothing

-- Just like p, except that if p fails, yield v instead
def :: a -> Parser a -> Parser a
def v p = p `orElse` pure v

-- Zero or more things matched by p
star :: Parser a -> Parser [a]
star p = (concWith (:) p (star p)) `orElse` pure []
-- One or more things matched by p
plus :: Parser a -> Parser [a]
plus p = concWith (:) p (star p)

-- like `andThen` but we only care about the first of the two
before :: Parser a -> Parser b -> Parser a
a `before` b = fmap fst (a `andThen` b)

-- like `andThen` but we only care about the second of the two
after :: Parser b -> Parser a -> Parser b
b `after` a = fmap snd (a `andThen` b)

-- x between p and q, but we don't care about p and q
between :: Parser xx -> Parser a -> Parser yy -> Parser a
between p x q = (x `before` q) `after` p

ws = tokenE " \t\n"
spacy p = enclosed_by ws p

eof :: Parser ()
eof = assert null

