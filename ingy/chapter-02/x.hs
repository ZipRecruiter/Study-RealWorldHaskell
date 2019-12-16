import Text.Printf (printf); import Criterion.Measurement (getTime)
main = do
  start <- getTime
  let len = length [1..]
  stop <- getTime
  printf "%.2f secs\n" $ stop - start
