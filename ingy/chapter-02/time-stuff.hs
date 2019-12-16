import Text.Printf (printf); import Criterion.Measurement (getTime)
main = do
  let list1 = [1..39999999]; list2 = [1..39999999]
  let l1 = length list1; l2 = length list2
  print "ok"
  time $ head $ drop (length list1 - 1) list1
  time $ head $ drop (length list2 - 1) list2
time f = do start <- getTime; print f; stop <- getTime; printf "%.2f secs\n" $ stop - start
