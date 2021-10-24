import Data.Char ( toLower )

scoreProductRec :: String -> Int
scoreProductRec "" = 1
scoreProductRec (x:xs) 
    | toLower x `elem` "haskell" = 10 * scoreProductRec xs
    | otherwise = scoreProductRec xs

scoreProductComp :: String -> Int
scoreProductComp str = product [10 | chr <- str, toLower chr `elem` "haskell"]

scoreProductFunc :: String -> Int
scoreProductFunc str = foldr (*) 1 (map f str)
    where
        f :: Char -> Int
        f x 
            | toLower x `elem` "haskell" = 10
            | otherwise = 1

test1 :: String -> Bool
test1 str = scoreProductComp str == scoreProductFunc str

calc :: Eq a => [a] -> Int
calc [] = 0
calc [_] = 0
calc (x:xs)
    | x `elem` xs = 1 + calc (rm x xs)
    | otherwise = calc xs
    where
        rm :: Eq a => a -> [a] -> [a]
        rm _ [] = []
        rm e (l:ls) 
            | e == l = rm e ls
            | otherwise = l : rm e ls
