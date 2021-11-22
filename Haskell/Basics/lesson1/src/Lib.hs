module Lib where

import Data.List()

myInt :: Integer
myInt = 5555555555555555555555555555555555555555555555555555555555555555555555555555555555555

double :: Integer -> Integer
double x = x+x

triple :: Integer -> Integer
triple x = x+x+x


maxi :: Integer -> Integer -> Integer
maxi x y = if x > y
                then x
                else y

maxi3 :: Integer -> Integer -> Integer -> Integer
maxi3 x y z = maxi x (maxi y z)

maxi3' :: Integer -> Integer -> Integer -> Integer
maxi3' x y z = if x > y
                then if x > z
                        then x
                        else z
                else if y > z
                        then y
                        else z

maxi3'' :: Integer -> Integer -> Integer -> Integer
maxi3'' x y z = 
    let 
        u = maxi x y
    in  
        maxi u z

maxi4 :: Integer -> Integer -> Integer -> Integer -> Integer 
maxi4 x y z t = 
    let 
        u = maxi3 x y z
    in 
        maxi u t

testMaxi4 :: Integer -> Integer -> Integer -> Integer -> Bool
testMaxi4 x y z t = 
    let 
        r = maxi4 x y z t
    in
        r >= x && r >= y && r >= z && r >= t


--------------------------------------------
-- other versions:

maxi3''' :: Integer -> Integer -> Integer -> Integer
maxi3''' x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | otherwise = z


--------------------------------------------

data Choice
    = Rock
    | Paper
    | Scissors
    deriving (Eq, Show)

data Result
    = Victory
    | Defeat
    | Draw
    deriving Show

instance Ord Choice where
    Rock <= Paper = True
    Paper <= Scissors = True
    Scissors <= Rock = True

game :: Choice -> Choice -> Result
game c1 c2
    |   c1 == Rock && c2 == Scissors
    ||  c1 == Scissors && c2 == Paper 
    ||  c1 == Paper && c2 == Rock
        = Victory
    |   c1 == c2 = Draw
    |   otherwise = Defeat

main1 :: IO()
main1 = print $ game Rock Scissors
