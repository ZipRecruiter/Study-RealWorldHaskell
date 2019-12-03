-- file: ch01/WC.hs
-- lines beginning with "--" are comments.

main = interact charCount

-- Returns 7
lineCount :: String -> [Char]
lineCount input = show (length (lines input)) ++ "\n"

-- Returns 14
wordCount :: String -> [Char]
wordCount input = show (length (words input)) ++ "\n"

-- Returns 121
charCount :: String -> [Char]
charCount input = show (length input) ++ "\n"
