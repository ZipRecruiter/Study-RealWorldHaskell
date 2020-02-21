module Main where

import TWorker (request)
import Control.Engine
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.HashTable.IO as H
import Data.IORef
import Data.Maybe

poolSize :: Int
poolSize = 8

type HashTable k v = H.CuckooHashTable k v

initTable :: IO (HashTable k v)
initTable = do
  h <- H.new
  return h

addAll :: (String -> IO (a)) -> [String] -> IO (Maybe a)
addAll a (x:xs) = do
  a x
  addAll a xs
addAll _ [] = return Nothing

test7 :: (String -> IO (a)) -> p -> String -> IO (Maybe [Char])
test7 add x url = do
  (status, nurl, newurls) <- request url
  addAll add newurls
  putStrLn $ (show status) ++ " " ++ nurl
  return Nothing

hookFilter :: (HashTable String Int) -> st -> String -> IO (Maybe String)
hookFilter h x y = do
  cval <- H.lookup h y
  H.insert h y 1
  if isNothing cval
    then return $ Just y
    else return Nothing

main :: IO ()
main = do
  rc <- newChan
  wc <- newChan
  go <- initEngine poolSize (poolSize * 8) (readChan rc) (writeChan wc) (test7 (writeChan rc)) ()
  pu <- initTable
  let filter = Hk (hookFilter pu) 1 "ensure uniq"
  addInputHook go filter
  writeChan rc "/"
  threadDelay maxBound
  putStrLn $ "DONE"
--  x <- request ["http://www.carrotmuseum.co.uk/index.html"] []
--  showReport x
