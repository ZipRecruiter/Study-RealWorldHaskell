-- Run with:
-- ./bin/ghcr -i ingy/chapter-04/a-4.hs transposeText < ingy/chapter-04/file1.txt

transposeText = unlines.transpose.lines

transpose xs = takeWhile (/="") $ map next [0..]
  where next i = foldr (++) "" $ map (take 1 . drop i) xs
