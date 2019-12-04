-- file: ch01/WC.hs
-- lines beginning with "--" are comments.

import System.Environment (getArgs)
import System.IO (stdin, hGetContents)
import Control.Monad (foldM, liftM)

perl_input =
  do
    args <- getFileArgs
    case args of
      [] -> hGetContents stdin
      a  -> foldM appendfile "" a  where
        appendfile s f = readFile f >>= return . (s ++)


startsWith x (h:_) = h == x
startsWith x [] = False

getFileArgs = liftM (dropWhile (startsWith '-')) getArgs
getFlags    = liftM (splitFlags . (takeWhile (startsWith '-'))) getArgs
  where
    splitFlags :: String -> [String]
    splitFlags f@['-', x] = [f]
    splitFlags ('-':x:y) = "-x" : splitFlags ('-' : y)



main = do
  
  perl_input >>= print . length
