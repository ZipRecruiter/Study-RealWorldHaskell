
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception.Base (Exception, throw)
import Control.Monad
import Data.Maybe (isJust)
import Data.Time

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

  -- when was this Taskpool created?
  -- this is so the Taskpool can be instrumented to
  -- announce stuff like "task X finished at time 1234"
  -- where the 1234 is relative to something in the recent past
  epoch :: UTCTime
  }

newTaskpool :: Integer -> IO Taskpool
newTaskpool n = do
  rem  <- newMVar n
  full <- newMVar ()
  empty <- newEmptyMVar
  start_time <- getCurrentTime
  return $ Taskpool { n_slots = n,
                      n_remaining = rem,
                      pool_full = full,
                      pool_empty = empty,
                      epoch = start_time
                    }

time :: Taskpool -> IO NominalDiffTime
time tp = do
  now <- getCurrentTime
  return $ diffUTCTime now (epoch tp)

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
  
-- release a task slot back into the pool
release :: Taskpool -> IO ()
release tp = do
  free_slots <- takeMVar (n_remaining tp)
  putMVar (n_remaining tp) (free_slots + 1)
  force_set_state tp
  
acquire :: Taskpool -> IO ()
acquire tp = do
  -- this can't complete until n_remaining is positive
  takeMVar (pool_full tp)
  
  free_slots <- takeMVar (n_remaining tp)
  putMVar (n_remaining tp) (free_slots - 1)
  force_set_state tp

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
  forkFinally task (\res -> release tp >> handle res)

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
