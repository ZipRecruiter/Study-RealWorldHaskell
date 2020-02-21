module TWorker (request, showReport) where

import Carrot
import Esrap

grabHrefs :: [Tag] -> [String]
grabHrefs (x:xs) = case length $ take 1 $ getAttr1 x "href" of
  0 -> [] ++ grabHrefs xs
  1 -> [normalizeUrl $ getAttr1 x "href"] ++ grabHrefs xs
grabHrefs [] = []

filterRequested :: [String] -> [String] -> [String]
filterRequested a b = foldr (\c f -> if (\x -> all (x/=) a) c then [c] ++ f else f) [] b

normalizeUrl :: String -> String
normalizeUrl s = s2
  where s' = if (take 29 s) == "http://www.carrotmuseum.co.uk"
             then drop 29 s
             else if null s
                  || (take 4 s == "http")
                  || (take 7 s == "mailto:")
                  || (head s == '#')
                  || (take 10 s == "javascript")
                then ""
                else s
        s'' = if (not $ null s') && (head s') /= '/'
              then "/" ++ s'
              else s'
        s1 = takeWhile (\c -> all (c/=) "?#") s''
        s2 = if length (take 5 s1) == 5 && take 5 (reverse s1) == "lmth."
             then s1
             else ""

--request :: [String] -> [String] -> IO [(Integer, String)]
--request (u:us) done = do
--  let nu = normalizeUrl u
--  req <- carrotReq nu
--  let tags  = attrFilter (tagFilter (tokenizeHTML (carrotData req)) "a") "href"
--      hrefs = us ++ grabHrefs tags 
--      filt  = filterRequested done hrefs
--  u' <- request filt (done ++ [nu])
--  return ( (carrotStatus req, nu) : u')
--request [] _ = return []

request :: String -> IO (Integer, String, [String])
request u = do
  let nu = normalizeUrl u
  req <- carrotReq nu
  let tags  = attrFilter (tagFilter (tokenizeHTML (carrotData req)) "a") "href"
      hrefs = grabHrefs tags
  return (carrotStatus req, nu, hrefs)

showReport :: [(Integer, String)] -> IO ()
showReport [] = putStrLn "!!!End of report" 
showReport (u:us) = do
  let status = fst u
      url    = snd u
  putStrLn $ (show status) ++ "  " ++ (if null url then "/" else url)
  showReport us

