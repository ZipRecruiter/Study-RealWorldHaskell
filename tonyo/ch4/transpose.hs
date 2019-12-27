import System.Environment (getArgs)
import Text.Printf

transpose :: [Char] -> [Char] -> [Char]
transpose (x:xs) (y:ys) = [x, y, '\n'] ++ (transpose xs ys)
transpose _ (y:ys) = [' ', y, '\n'] ++ (transpose [] ys)
transpose (x:xs) _ = [x, ' ', '\n'] ++ (transpose xs [])
transpose _ _ = []

showFirstLast :: [String] -> IO ()
showFirstLast [] = printf "done\n"
showFirstLast (x:[]) = do
    printf "trailing line: %s\n" x
    showFirstLast []
showFirstLast (x:y:xs)  = do
    print tx
    showFirstLast xs
    where
        tx = transpose x y


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
