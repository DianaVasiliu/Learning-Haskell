import Data.List
import Data.Char

-- ex1

-- a)
-- the empty string is the case of stopping, it does not turn into anything
-- for each character c in the string,
-- if it is lowercase, we turn it to uppercase and continue the recursion
-- if it is a capital letter, we turn it into a lower case and continue the recursion
-- if it is a different type of character than letter, then it remains the same and we continue the recursion
transformRec :: String -> String
transformRec "" = ""
transformRec (c:str) 
    | isLower c = toUpper c : transformRec str
    | isUpper c = toLower c : transformRec str
    | otherwise = c : transformRec str

-- b)
-- we take each character from the string and apply the transformation function in uppercase and vice versa
-- a list of characters is thus formed, so a String
transformComp :: String -> String
transformComp str = [f c | c <- str]
    where
        f :: Char -> Char
        f c 
            | isLower c = toUpper c
            | isUpper c = toLower c
            | otherwise = c

-- c)
-- we apply the f function to each character in the input string, using the map function
-- and the function f transforms a capital letter into a lowercase letter and vice versa,
transformFunc :: String -> String
transformFunc = map f
    where
        f :: Char -> Char
        f c 
            | isLower c = toUpper c
            | isUpper c = toLower c
            | otherwise = c


test1 = transformRec "It's 10:00 O'Clock THURSDAY September 2" == "iT'S 10:00 o'cLOCK thursday sEPTEMBER 2"

test2 = transformRec "5, 10, 15, 25 are NUmbERS DiviSible BY 5!!" == "5, 10, 15, 25 ARE nuMBers dIVIsIBLE by 5!!"

test3 = transformComp "It's 10:00 O'Clock THURSDAY September 2" == "iT'S 10:00 o'cLOCK thursday sEPTEMBER 2"

test4 = transformFunc"It's 10:00 O'Clock THURSDAY September 2" == "iT'S 10:00 o'cLOCK thursday sEPTEMBER 2"

test5 = transformFunc "!!! ANNOUNCEMENT !!! ~It's Time For Exam~" == "!!! announcement !!! ~iT'S tIME fOR eXAM~"

-- *Main Test.QuickCheck> quickCheck test
-- +++ OK, passed 100 tests.
test str = transformComp str == transformFunc str && transformFunc str == transformRec str

-- ex2
data Pol 
    = X Integer -- represents the X variable raised to a (>=0) power
    | S Integer -- scalar
    | Pol :+: Pol -- sum
    | Pol :*: Pol -- product
    deriving (Show)

polynome = (S 1 :*: X 7) :+: (S 2 :*: X 5) :+: (S 7 :*: X 3) :+: (S 10 :*: X 0)
polynome2 = S 1 :+: X 7 :*: S 2 :+: X 5
polynome3 = S 1 :*: X 3 :*: S 4
polynome4 = S 3
polynome5 = X 4
polynome6 = X 4 :*: S 9
polynome7 = S 1 :+: S 2 :+: S 3
polynome8 = X 2 :+: X 4
polynome9 = S 1 :*: X 10
polynome10 = S 3 :*: X 13 :+: S 4 :+: X 4

-- function that forms the list of integers that represent scalars in a polynome
getScalars :: Pol -> [Integer]
getScalars (X _) = []
getScalars (S i) = [i]
getScalars (p1 :+: p2) = getScalars p1 ++ getScalars p2
getScalars (p1 :*: p2) = getScalars p1 ++ getScalars p2

-- function that forms the list of integers that represent coefficients in a polynome
getCoefficients :: Pol -> [Integer]
getCoefficients (X i) = [i]
getCoefficients (S _) = []
getCoefficients (p1 :+: p2) = getCoefficients p1 ++ getCoefficients p2
getCoefficients (p1 :*: p2) = getCoefficients p1 ++ getCoefficients p2

-- function that takes as a parameter a polynome and returns a list of strings
-- - which represents the form of the polynomial, with the terms and operations that appear, in the order in which they appear
-- without taking into account the parentheses
-- example: shape polynome2 = ["S","+","X","*","S","+","X"]
shape :: Pol -> [String]
shape (X _) = ["X"]
shape (S _) = ["S"]
shape (p1 :+: p2) = shape p1 ++ ["+"] ++ shape p2
shape (p1 :*: p2) = shape p1 ++ ["*"] ++ shape p2

shapeOK :: Pol -> Bool
shapeOK pol = f terms
    where
        terms = shape pol
        -- a polynome shape is wrong if it has exactly 1, 2 or 4 elements (terms or operations)
        f :: [String] -> Bool
        f [] = True
        f [x] = False
        f [x, y] = False
        -- if it has 3 elements, then they must be of type S * X
        f [x, y, z] = x == "S" && y == "*" && z == "X"
        f [_, _, _, _] = False
        -- we take "pairs" of 4 elements from the list of elements of the polynomial
        --  a polynomial is wrong for any of the cases in which:
        --      2 consecutive operations are identical
        --      2 consecutive terms are identical (the two lines commented in the definition of the function)
        --  the polynomial is correct if, for a tuple (x, y, z, t),
        --  x is S, y is *, z is X (ie S * X type operation occurs),
        --  and after this operation + appears
        --  if all these conditions are met, then the recursion can be continued
        --  in the rest of the polynomial (excluding the operation "S * X +")
        --  in all other cases, the polynomial is wrong
        f (x:y:z:t:xs)
            | y == "*" && t == "+" = x == "S" && z == "X" && f xs
            -- | y == t = False    
            -- | x == z = False
            | otherwise = False

verify :: Pol -> Bool
-- a polynomial has a good shape if:
--      the scalars that appear satisfy the given condition (they are non-zero)
--      the coefficients that appear satisfy the given condition (they appear in descending order)
--      the shape of the polynomial is the required one
verify pol = scalarsOK && coefficientsOK && shapeOK pol
    where
        -- we extract scalars and coefficients from the polynome
        scalari = getScalars pol
        coeficienti = getCoefficients pol

        -- scalars are ok if they are all different from 0
        scalarsOK = not $ 0 `elem` scalari

        -- the coefficients are ok if they appear in descending order
        coefficientsOK = sortedDesc coeficienti

        -- function that checks if a list of integers is sorted strictly descending
        sortedDesc :: [Integer] -> Bool
        sortedDesc [] = True
        sortedDesc [_] = True
        sortedDesc (x:y:xs) 
            | x > y = sortedDesc (y:xs)
            | otherwise = False

test6 = verify polynome == True
test7 = verify polynome2 == False
test8 = verify polynome3 == False
test9 = verify polynome4 == False
test10 = verify polynome5 == False
test11 = verify polynome6 == False
test12 = verify polynome7 == False
test13 = verify polynome8 == False
test14 = verify polynome9 == True
test15 = verify polynome10 == False

-- Note: 
-- There is a small problem regarding the shape function
-- it will return True for a polynomial of the following shape:
--  (S 1 :*: (X 2 :+: S 2)) :*: X 1
-- so it's note entirely correct

-- ex3

data Digit = O | I | D                  -- O=0, I=1, D=2
    deriving Show
data NBase = NB Digit Digit Digit       -- numbers written in base 3 using 3 
    deriving Show

class MyNum x where
    myfromInteger :: Integer -> x
    plus :: x -> x -> x


instance MyNum NBase where
    myfromInteger x 
        | x < 0 = error "The number cannot be transformed"
        | otherwise = 
            -- the number must be 3 digits in base 3, otherwise it cannot be represented
            if length repres /= 3
                then error "The number cannot be represented using 3 bits"
                -- if it is a 3 digit number, then we take each digit from the representation and turn it into Digit
                else NB (toDigit $ repres !! 0) (toDigit $ repres !! 1) (toDigit $ repres !! 2)
        where
            repres = fill (transform x)
            -- we transform the number from base 10 to base 3
            -- and save the numbers from base 3 in order in a list
            transform :: Integer -> [Integer]
            transform 0 = []
            transform x = transform (x `div` 3) ++ [x `rem` 3]

            -- if the number in base 3 has less than 3 digits, then we add 0 at the beginning
            -- until we have 3 digits
            fill :: [Integer] -> [Integer]
            fill [] = [0, 0, 0]
            fill [x] = [0, 0, x]
            fill [x, y] = [0, x, y]
            fill x = x

            -- we transform the digits in base 3 into the Digit data type
            toDigit :: Integer -> Digit
            toDigit 0 = O
            toDigit 1 = I
            toDigit 2 = D
            toDigit _ = error "It's not a digit in base 3"

    -- we transform the numbers x and y into base 10, we add them, then we transform back into base 3
    plus x y = myfromInteger (mytoInteger x + mytoInteger y)
        where
            -- we transform the number from the NBase data type into base 10
            mytoInteger :: NBase -> Integer
            mytoInteger (NB x y z) = (val x * 3 ^ 2) + (val y * 3 ^ 1) + val z

            -- we transform a Digit into the corresponding value in base 3
            val :: Digit -> Integer
            val O = 0
            val I = 1
            val D = 2

-- Eq instances for the tests
instance Eq Digit where
    O == O = True
    I == I = True
    D == D = True
    _ == _ = False

instance Eq NBase where
    (NB a b c) == (NB x y z) = a == x && b == y && c == z

test16 = plus (NB I I O) (NB I D O) -- throws error, being the value 12+15=27=1000(3)
test17 = plus (NB D O O) (NB O I I) == NB D I I -- 18+4=22=211(3)
test18 = plus (NB I I I) (NB O O D) == NB I D O -- 13+2=15=120(3)
