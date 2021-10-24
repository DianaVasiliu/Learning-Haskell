module Lesson2.Src.Lib where

import Data.List ()
import Data.Char ( digitToInt, isDigit )
-- import Test.QuickCheck

-----------------------------------------------
------------- FIBONACCI RECURSION -------------
-----------------------------------------------

fibonacci :: Integer -> Integer
fibonacci n
    | n < 2     = n
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

fibonacci' :: Integer -> Integer
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' n =
    fibonacci' (n - 1) + fibonacci' (n - 2)

-- testing the equivalence of the 2 functions above:
-- testFibo :: Integer -> Property
-- testFibo n = n >= 0 && n < 25 ==> fibonacci n == fibonacci' n

-- in terminal:
-- import Test.QuickCheck
-- quickCheck testFibo

-- Linear Fibonacci:
fibonacciLinear :: Integer -> Integer
fibonacciLinear 0 = 0
fibonacciLinear n = snd (fibonacciPair n) 
    where
        fibonacciPair :: Integer -> (Integer, Integer)
        fibonacciPair 1 = (0, 1)
        fibonacciPair n =
            let (x, y) = fibonacciPair (n - 1)
            in (y, x + y)

-- testing the running time between the linear and non-linear fibonacci functions:
-- fibonacci 30
-- fibonacciLinear 1000

------------------------------------------
------------- LIST RECURSION -------------
------------------------------------------

semiEvenRecursive1 :: [Int] -> [Int]
semiEvenRecursive1 l
    | null l = l
    | even h = h `div` 2 : t'
    | otherwise = t'
    where
        h = head l
        t = tail l
        t' = semiEvenRecursive1 t

-- prefered:
semiEven :: [Int] -> [Int]
semiEven [] = []
semiEven (h : t)
    | even h = h `div` 2 : t'
    | otherwise = t'
    where
        t' = semiEven t

----------------------------------------------
------------- LIST COMPREHENSION -------------
----------------------------------------------

semiEvenComprehension :: [Int] -> [Int]
semiEvenComprehension l = [x `div` 2 | x <- l, even x]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec lb ub (h : t) 
    | lb <= h && h <= ub    = h : t'
    | otherwise             = t'
    where
        t' = inIntervalRec lb ub t

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp lb ub l = [x | x <- l, lb <= x && x <= ub]

positivesRec :: [Int] -> Int
positivesRec [] = 0
positivesRec (x:xs) 
    | x > 0 = 1 + p
    | otherwise = p
    where
        p = positivesRec xs

positivesComp :: [Int] -> Int
positivesComp l = length [x | x <- l, x > 0]

getPosition :: [Int] -> Int -> [Int]
getPosition l pos
    | odd x = pos : result
    | otherwise = result
    where 
        x = head l
        t = tail l
        next = pos + 1
        result = getPosition t next

oddPositionsRec :: [Int] -> [Int]
oddPositionsRec [] = []
oddPositionsRec l = getPosition l 0

oddPositionsComp :: [Int] -> [Int]
oddPositionsComp l = [i | (x, i) <- zip l [0..], odd x]

digitsProductRec :: String -> Int
digitsProductRec "" = 1
digitsProductRec (x:xs)
    | isDigit x = digitToInt x * digitsProductRec xs
    | otherwise = digitsProductRec xs

digitsProductComp :: String -> Int
digitsProductComp s = product [digitToInt ch | ch <- s, isDigit ch]

discountRec :: [Float] -> [Float]
discountRec [] = []
discountRec (x:xs) 
    | discountedPrice < 200 = discountedPrice : discountRec xs
    | otherwise = discountRec xs
    where
        discountedPrice = 0.75 * x

discountComp :: [Float] -> [Float]
discountComp l = [discountedPrice | x <- l, let discountedPrice = 0.75 * x, discountedPrice < 200]
