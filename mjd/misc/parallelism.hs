import Control.Concurrent (forkIO, forkFinally, threadDelay)
import Control.Concurrent.MVar
import Control.Exception.Base (Exception, throw)

data MyException = MyException deriving Show
instance Exception MyException

mseq [] = return []
mseq (a:as) = do
  av  <- a
  avs <- mseq as
  return (av:avs)

slow_add :: Num b => b -> b -> IO b
slow_add a b = do
  threadDelay 2000000
  return $ a + b

get_or_rethrow :: Exception e => (Either e a) -> a
get_or_rethrow (Left exc) = throw exc
get_or_rethrow (Right v) = v

in_parallel_2 :: IO a -> IO b -> IO (a, b)
in_parallel_2 a b = do
  a_var <- newEmptyMVar
  b_var <- newEmptyMVar
  a_thread <- forkFinally a (putMVar a_var . get_or_rethrow)
  b_thread <- forkFinally b (putMVar b_var . get_or_rethrow)
  
  a_result <- readMVar a_var
  b_result <- readMVar b_var
  return (a_result, b_result)

do_it = in_parallel_2 (slow_add 1 2) (slow_add 3 4)
fail_l = in_parallel_2 (throw MyException) (slow_add 3 4)
fail_r = in_parallel_2 (slow_add 3 4) (throw MyException) 


in_parallel :: [IO a] -> IO [MVar a]
in_parallel [] = return []
in_parallel (a:as) = do
  a_var  <- newEmptyMVar
  forkFinally a (putMVar a_var . get_or_rethrow)
  as <- in_parallel as
  return $ a_var : as

parallel_show [] = return ()
parallel_show (a:as) = do
  z <- readMVar a
  print z
  parallel_show as

class Printable a where
  print_it :: a -> IO ()

instance Show a => Printable (MVar a) where
  print_it v = readMVar v >>= print

instance Printable a => Printable [a] where
  print_it ls = sequence_ $ map print_it ls

-- instance Show a => Printable (IO a) where
--  print_it = (>>= print)
  
-- run a bunch of actions in parallel
-- their results are available in the MVars
pmap :: (a -> IO b) -> [a] -> IO [MVar b]
pmap f ls = in_parallel (map f ls)



-- pmap' :: Int -> (a -> IO b) -> IO [b]
-- pmap' n f ls = do



try_it = pmap example [1..12] >>= parallel_show where
  example = \a -> do threadDelay (50000); return (a*11)
  
