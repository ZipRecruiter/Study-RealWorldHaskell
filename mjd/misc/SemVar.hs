
module SemVar (SemVar,
               newSemVar,
               mutateSemVar,
               incrementSemVar, decrementSemVar)
       where
import Control.Concurrent.MVar

data SemVar a = SemVar { semaphore :: MVar (), value :: MVar a }

newSemVar :: a -> IO (SemVar a)
newSemVar v = do
  sv <- newEmptyMVar
  vv <- newMVar v
  return SemVar { semaphore = sv,
                  value     = vv }

mutateSemVar :: SemVar a -> (a -> a) -> IO ()
mutateSemVar sv f = do
  putMVar (semaphore sv) ()
  v <- takeMVar (value sv)
  putMVar (value sv) (f v)
  takeMVar (semaphore sv)
  return ()
       
  
incrementSemVar sv = mutateSemVar sv (+ 1)
decrementSemVar sv = mutateSemVar sv (subtract 1)
