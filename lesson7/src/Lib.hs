import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.List
import Data.Char

double :: Int -> Int
double n = 2 * n

triple :: Int -> Int
triple n = 3 * n

penta :: Int -> Int
penta n = 5 * n

test :: Int -> Bool
test x = (double x + triple x) == penta x

wrongTest :: Int -> Bool
wrongTest x = (double x - triple x) == penta x

myLookUp :: Int -> [(Int,String)]-> Maybe String
myLookUp _ [] = Nothing
myLookUp x ((k, v) : xs) 
    | x == k = Just v
    | otherwise = myLookUp x xs

testLookUp :: Int -> [(Int,String)] -> Bool
testLookUp x xs = myLookUp x xs == lookup x xs

testLookUpCond :: Int -> [(Int,String)] -> Property
testLookUpCond n list = n > 0 && n `div` 5 == 0 ==> testLookUp n list

myLookUp' :: Int -> [(Int,String)]-> Maybe String
myLookUp' _ [] = Nothing
myLookUp' x ((k, v) : xs) 
    | x == k && v == "" = Just ""
    | x == k            = 
        let (letter : string) = v
        in Just (toUpper letter : string)
    | otherwise         = myLookUp' x xs

testMyLookUp' :: Int -> [(Int,String)] -> Property 
testMyLookUp' n list = all isCapitalizedSnd list ==> myLookUp' n list == lookup n list
    where
        isCapitalizedSnd :: (Int, String) -> Bool
        isCapitalizedSnd (_, "") = True
        isCapitalizedSnd (_, x : _) = isUpper x

data ElemIS = I Int | S String
    deriving (Show, Eq)

instance Arbitrary ElemIS where
    arbitrary = oneof [genI, genS]
        where
            f = unGen (arbitrary :: Gen Int)
            g = unGen (arbitrary :: Gen String)
            genI = MkGen (\s i -> let x = f s i in I x)
            genS = MkGen (\s i -> let x = g s i in S x)

myLookUpElem :: Int -> [(Int,ElemIS)]-> Maybe ElemIS
myLookUpElem _ [] = Nothing
myLookUpElem n ((k, v) : xs) 
    | n == k = Just v
    | otherwise = myLookUpElem n xs

testLookUpElem :: Int -> [(Int,ElemIS)] -> Bool
testLookUpElem n lst = myLookUpElem n lst == lookup n lst
