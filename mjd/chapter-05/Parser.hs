module Parser
    (
      Parser
    , andThen
    , orElse, alternatives
    , lit, charser, token, tokenE, charclass, ws
    , fails
    , concWith, seqStrings
    , pseq, seqp2, seqp3, seqp4, seqp5
    , optional
    , assert
    , def
    , star, plus
    , (<|>)
    , eof
    , before, after, between, enclosed_by, spacy
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

-- Here are a bunch of parsers that each return a string.
-- Run them in sequence, return the concatenation of the results
-- if x is empty, the resulting parser always succeeds and returns ""
seqStrings :: [Parser String] -> Parser String
seqStrings x = foldr (concWith (++)) (pure "") x


fails = P $ const Nothing

concWith f p1 p2 = do
  v1 <- p1
  v2 <- p2
  return $ f v1 v2

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
a `before` b = (a `andThen` b) <|> fst

-- like `andThen` but we only care about the second of the two
after :: Parser b -> Parser a -> Parser b
b `after` a = (a `andThen` b) <|> snd

-- x between p and q, but we don't care about p and q
between :: Parser xx -> Parser a -> Parser yy -> Parser a
between p x q = (x `before` q) `after` p

enclosed_by :: Parser x -> Parser a -> Parser a
enclosed_by x p = between x p x

ws = tokenE " \t\n"
spacy p = enclosed_by ws p

-- like parser p, but transform the result value with f
(<|>) :: Parser a -> (a -> b) -> Parser b
p <|> f = fmap f p

eof :: Parser ()
eof = assert null

pseq :: [Parser a] -> Parser [a]
pseq ps = foldr (concWith (:)) (pure []) ps

seqp2 :: (a -> b -> z)           -> Parser a -> Parser b -> Parser z
seqp2 f pa pb = do
  a <- pa
  b <- pb
  return $ f a b

seqp3 :: (a -> b -> c -> z)      -> Parser a -> Parser b -> Parser c -> Parser z
seqp3 f pa pb pc = do
  a <- pa
  b <- pb
  c <- pc
  return $ f a b c

seqp4 :: (a -> b -> c -> d -> z) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser z
seqp4 f pa pb pc pd = do
  a <- pa
  b <- pb
  c <- pc
  d <- pd
  return $ f a b c d

seqp5 :: (a -> b -> c -> d -> e -> z) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e ->Parser z
seqp5 f pa pb pc pd pe = do
  a <- pa
  b <- pb
  c <- pc
  d <- pd
  e <- pe
  return $ f a b c d e
