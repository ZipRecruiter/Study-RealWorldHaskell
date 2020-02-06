module Carrot (carrotReq, carrotStatus, carrotData) where

import Control.Exception

data Carrot = Carrot Integer String
              deriving (Show)

carrotStatus :: Carrot -> Integer
carrotStatus (Carrot x _) = x

carrotData :: Carrot -> String
carrotData (Carrot _ x) = x

carrotReq :: String -> IO (Carrot)
carrotReq fn = do
  let hu = if (take 29 fn) == "http://www.carrotmuseum.co.uk"
           then drop 29 fn
           else fn
      fp = if hu == "/"
           then "./www/index.html"
           else "./www/"
  fc <- try (readFile fp)
  case fc of
    Left  e -> return $ Carrot 404 (show (e :: SomeException))
    Right c -> return $ Carrot 200 c
