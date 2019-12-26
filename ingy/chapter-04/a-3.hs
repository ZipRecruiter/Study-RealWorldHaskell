-- Run with:
-- ./bin/ghcr -i ingy/chapter-04/a-3.hs firstWords < ingy/chapter-04/a-3.hs
firstWords = unlines.(map firstWord).lines

firstWord [] = []
firstWord xs = (head.words) xs
