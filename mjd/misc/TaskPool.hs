
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception.Base (Exception, throw)
import Control.Monad
import Data.Maybe (isJust)

{-
  Idea: a taskpool has a controlled variable that counts how many more tasks
  it can run before it must block.

  It can be asked to run another task; if the pool is full it blocks,
  and if not it starts the task and returns a threadID.

  When the task ends, it decrements its counter.

  It's the task's responsibility to deposit the result somewhere it can be read.
-}

data Taskpool = Taskpool {
  -- maximum number of task slots that can be allocated
  n_slots :: Integer,
  
  -- number of unallocated slots
  n_remaining :: MVar Integer,

  -- this MVar is empty if all slots are allocated
  -- (that is, if n_remaining is zero)
  -- and contains () otherwise
  pool_full :: MVar (),
  -- similarly this one is empty if no slots are allocated
  pool_empty :: MVar (),

  -- Mutex controlling access to the taskpool's internals
  semaphore :: MVar ()
  }

newTaskpool :: Integer -> IO Taskpool
newTaskpool n = do
  rem  <- newMVar n
  full <- newMVar ()
  empty <- newEmptyMVar
  sem  <- newMVar ()
  return $ Taskpool { n_slots = n,
                      n_remaining = rem,
                      pool_full = full,
                      pool_empty = empty,
                      semaphore = sem }


slots :: Taskpool -> IO (Integer, Integer)
slots tp = readMVar (n_remaining tp) >>= \n_rem -> return (n_slots tp, n_rem)

-- doesn't matter if these fail, that means it already contained
-- the values we wanted
force_clear_flag :: (Taskpool -> MVar ()) -> Taskpool -> IO ()
force_clear_flag flag tp = tryPutMVar  (flag tp) () >> return ()
force_set_flag   :: (Taskpool -> MVar ()) -> Taskpool -> IO ()
force_set_flag   flag tp = tryTakeMVar (flag tp)    >>  return ()

force_set_state :: Taskpool -> IO ()
force_set_state tp = do
  (total, remaining) <- slots tp
--  print $ "> pool empty slots: " ++ (show remaining)
  if remaining == 0 then do
--    print "> pool is full"
    force_set_flag   pool_full  tp 
    force_clear_flag pool_empty tp
  else if remaining == total then do
--    print "> pool is empty"
    force_clear_flag pool_full  tp 
    force_set_flag   pool_empty tp
  else do
    force_clear_flag pool_full  tp
    force_clear_flag pool_empty tp
--  print $ ">>> " ++ (show remaining) ++ "/" ++ (show total) ++ "\n"
  return ()

wait_until_flag_set flag tp = do
  putMVar (flag tp) ()

wait_until_flag_clear flag tp = do
  takeMVar (flag tp)
  
-- perform some action on a Taskpool,
-- acquiring the Taskpool's mutex first,
-- and releasing it afterward
withTP :: String -> Taskpool -> IO a -> IO a
withTP label tp action = action

_withTP label tp action = do
  takeMVar (semaphore tp)
  z <- action
  putMVar (semaphore tp) ()
  return z

-- release a task slot back into the pool
release :: Taskpool -> IO ()
release tp = withTP "release" tp $ do
--  print "+1+"
  free_slots <- takeMVar (n_remaining tp)
--  print "+2+"
  putMVar (n_remaining tp) (free_slots + 1)
--  print "+3+"
  force_set_state tp
  
acquire :: Taskpool -> IO ()
acquire tp = withTP "acquire" tp $ do
--  print "-1-"
  -- this can't complete until n_remaining is positive
  takeMVar (pool_full tp)
  
--  print "-2-"
  free_slots <- takeMVar (n_remaining tp)
--  print "-3-"
  putMVar (n_remaining tp) (free_slots - 1)
--  print "-4-"
  force_set_state tp
--  print "-5-"

is_empty :: Taskpool -> IO Bool
is_empty tp = do
  (_, used) <- slots tp
  return $ used == 0

wait_for_completion :: Taskpool -> IO ()
wait_for_completion tp = wait_until_flag_set pool_empty tp

handle (Left exc) = throw exc
handle (Right _)  = return ()

start_task :: Taskpool -> IO a -> IO ThreadId
start_task tp task = do
  acquire tp
  forkFinally task (\res -> print "***" >> release tp >> handle res)

slow_task name decisecs = do
--  print $ "Starting '" ++ name ++ "'"
  threadDelay (decisecs * 100000)
  print $ "Finished '" ++ name ++ "'"

main = do 
  tp <- newTaskpool 2
  start_task tp (slow_task "a" 5)
  start_task tp (slow_task "b" 25)
  start_task tp (slow_task "c" 15)
  wait_for_completion tp
  print "******Exiting."
