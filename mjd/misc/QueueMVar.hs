

module QueueMVar where

import Data.Maybe
import Control.Concurrent.MVar

verbose = False

diag s =
  if verbose then print s
  else nothing

data Queue a = Queue {
  -- queued data
  front :: MVar [a],
  back  :: MVar [a],

  -- empty if the queue is empty,
  -- contains () otherwise
  is_empty :: MVar ()
  }

display q = do
  f <- readMVar (front q)
  b <- readMVar (back q)
  mt <- queue_is_empty q
  diag $ "Queue contains: front(" ++ (show f) ++ ") back(" ++ (show b) ++ ") " 
    ++ "empty=" ++ (show mt)
  
newEmptyQueue :: IO (Queue a)
newEmptyQueue = do
  f <- newMVar []
  b <- newMVar []
  mt <- newEmptyMVar
  return $ Queue {
    front=f, back=b, is_empty=mt
    }

-- queue initialized with some list of things
newQueue :: [a] -> IO (Queue a)
newQueue [] = newEmptyQueue
newQueue ls = do
  f <- newMVar ls
  b <- newMVar []
  mt <- newMVar ()
  return $ Queue {
    front=f, back=b, is_empty=mt
    }

queue_is_empty :: Queue a -> IO Bool
queue_is_empty q = 
  isNothing <$> tryReadMVar (is_empty q)

-- record that this queue is not empty
force_nonempty :: Queue a -> IO ()
force_nonempty q = do
  discard $ tryPutMVar (is_empty q) ()

wait_until_nonempty :: Queue a -> IO ()
wait_until_nonempty q = takeMVar (is_empty q)
  
-- run an action and throw away the result
discard :: IO a -> IO ()
discard = (>> (return ()))

-- do nothing
nothing :: IO ()
nothing = return ()

-- adjust the is_empty flag of a queue
-- to accurate reflect whether it has
-- any queued data
force_set_nonempty :: Queue a -> IO ()
force_set_nonempty q = do
  f <- takeMVar (front q)
  if null f then
    discard $ tryTakeMVar (is_empty q) 
  else
    discard $ tryPutMVar (is_empty q) ()
  putMVar (front q) f
  return ()

-- add an item to the back of a queue
push :: Show a => Queue a -> a -> IO ()
push q a = do
  b <- takeMVar (back q)
  putMVar (back q) (a:b)
  diag $ "pushed " ++ (show a)
  display q
  force_nonempty q

-- this whole thing is a race condition.
-- queue needs a mutex
pop :: Show a => Queue a -> IO a
pop q = do
  wait_until_nonempty q

  (t:ts) <- takeMVar $ front q

  -- now remove the first item from the front,
  -- moving back items to the front if that would
  -- make the front empty
  if (null ts) then do
    b <- takeMVar $ back q
    putMVar (back q) []
    -- I'm a race condition
    putMVar (front q) (reverse b)
  else do
    -- I'm a race condition
    putMVar (front q) ts

  diag $ "popped " ++ (show t)
  display q

  force_set_nonempty q  

  return t

main = do
  q <- newQueue [1]
  push q 2
  pop q >>= print
  push q 3
  push q 4
  pop q >>= print
  push q 5
  pop q >>= print
  pop q >>= print
  pop q >>= print
--  pop q >>= print
  print "** finished"
