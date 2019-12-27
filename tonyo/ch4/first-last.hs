import System.Environment (getArgs)
import Text.Printf

showFirstLast :: [String] -> IO ()
showFirstLast [] = printf "done\n"
showFirstLast ([]:xs) = do
    printf "(empty line)\n"
    showFirstLast xs
showFirstLast (x:xs)  = do
    printf "'%s', '%s'\n" first lastx
    showFirstLast xs
    where
        wx    = words x
        first = head wx
        lastx = last wx

splitShow :: String -> IO ()
splitShow inputFile = do
    fileData <- readFile inputFile
    let linex = lines fileData
    showFirstLast linex

main = do
    args <- getArgs
    case args of
        [input] -> splitShow input
        _       -> printf "error: exactly one argument needed\n"
