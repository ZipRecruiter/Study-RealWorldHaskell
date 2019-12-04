-- file: ch01/WC.hs
-- lines beginning with "--" are comments.

import System.Environment (getArgs)
import System.IO (stdin, hGetContents)
import Control.Monad (foldM)

perl_input =
  do
    args <- getArgs
    case args of
      [] -> hGetContents stdin
      a  -> foldM appendfile "" a  where
        appendfile s f = readFile f >>= return . (s ++)

main = do
  perl_input >>= print . length
