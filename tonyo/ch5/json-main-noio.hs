import YAJP7

main = do
  interact $ show . yajp7_fparse
  putStrLn ""
