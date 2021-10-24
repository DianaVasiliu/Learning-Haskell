import Data.List
import GHC.Natural ( Natural )


productRec :: [Integer] -> Integer
productRec [] = 1
productRec (x:xs) = x * productRec xs

productFold :: [Integer] -> Integer
productFold = foldr (*) 1

andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold = foldr (&&) True 

concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

rmChar :: Char -> String -> String
rmChar _ "" = ""
rmChar ch (s:str)
    | ch == s = rmChar ch str
    | otherwise = s : rmChar ch str

rmChar' :: Char -> String -> String
rmChar' c = filter (c /=)

rmChar'' :: Char -> String -> String
rmChar'' a b = [x | x <- b, x /= a]

rmCharsRec :: String -> String -> String
rmCharsRec "" str = str
rmCharsRec (x : xs) str = rmChar x (rmCharsRec xs str)

test_rmchars :: Bool
test_rmchars = rmCharsFold ['a'..'l'] "football" == "oot"

rmCharsFold :: String -> String -> String
rmCharsFold rm str = foldr rmChar str rm

---------------------------------------
---------- LAZY EVALUATION ------------
---------------------------------------
logistic :: Num a => a -> a -> Natural -> a
logistic rate start = f
    where
        f 0 = start
        f n = rate * f (n - 1) * (1 - f (n - 1))

logistic0 :: Fractional a => Natural -> a
logistic0 = logistic 3.741 0.00079

ex1 :: Natural
ex1 = 14

ex20 :: Fractional a => [a]
ex20 = [1, logistic0 ex1, 3]

ex21 :: Fractional a => a
ex21 = head ex20

ex22 :: Fractional a => a
ex22 = ex20 !! 2 

ex23 :: Fractional a => [a]
ex23 = drop 2 ex20 -- drop n list -> scoate primele n elemente din list

ex24 :: Fractional a => [a]
ex24 = tail ex20

ex31 :: Natural -> Bool
ex31 x = x < 7 || logistic0 (ex1 + x) > 2

ex32 :: Natural -> Bool
ex32 x = logistic0 (ex1 + x) > 2 || x < 7

ex33 :: Bool
ex33 = ex31 5

ex34 :: Bool
ex34 = ex31 7

ex35 :: Bool
ex35 = ex32 5

ex36 :: Bool
ex36 = ex32 7

-------------------------------------------
----------- FOLDR UNIVERSALITY ------------
-------------------------------------------

foldr_ :: (a -> b -> b) -> b -> ([a] -> b)
foldr_ op unit = f
    where
        f [] = unit
        f (a : as) = a `op` f as

oddSquaresSum :: [Integer] -> Integer
oddSquaresSum [] = 0
oddSquaresSum (a : as)
    | odd a = a * a + oddSquaresSum as
    | otherwise = oddSquaresSum as

oddSquaresSumFold :: [Integer] -> Integer
oddSquaresSumFold = foldr op unit
    where
        unit = 0
        a `op` suma
            | odd a = a * a + suma
            | otherwise = suma

map_ :: (a -> b) -> [a] -> [b]
map_ f [] = []
map_ f (a : as) = f a : map_ f as

mapFold :: (a -> b) -> [a] -> [b]
mapFold f = foldr op unit
    where
        unit = []
        a `op` l = f a : l

filter_ :: (a -> Bool) -> [a] -> [a]
filter_ p [] = []
filter_ p (a : as)
    | p a = a : filter_ p as
    | otherwise = filter_ p as

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold p = foldr op unit
    where
        unit = []
        a `op` filtered
            | p a = a : filtered
            | otherwise = filtered

sign :: [Integer] -> String
sign [] = ""
sign (x : xs) 
    | x >= -9 && x < 0  = "-" ++ sign xs
    | x > 0 && x <= 9   = "+" ++ sign xs
    | x == 0            = "0" ++ sign xs
    | otherwise         = sign xs

test_sign :: Bool
test_sign = sign [5, 10, -5, 0] == "+-0" -- 10 is ignored

signFold :: [Integer] -> String
signFold = foldr op unit
    where
        unit = ""
        x `op` res
            | x >= -9 && x < 0  = "-" ++ res
            | x > 0 && x <= 9   = "+" ++ res
            | x == 0            = "0" ++ res
            | otherwise         = res

test_sign_fold :: Bool
test_sign_fold = signFold [3, 6, -2, -10, 0, 20] == "++-0"
