
import Data.Char (isSpace)

myAny :: ( a -> Bool ) -> [a] -> Bool
myAny f ls = foldr (\a b -> b || f a) False ls

myCycle :: [a] -> [a]
myCycle ls = foldr (\a b -> a ++ b) (myCycle ls) [ls]

myWords :: String -> [String]
myWords str = filter (/= "") $
                foldr (\a (w:ws) -> if isSpace a then []:w:ws else (a:w):ws)
                  [[]] str

myUnwords :: [String] -> String
myUnwords (w:ws) = foldl (\b a -> b ++ (' ':a)) w ws
