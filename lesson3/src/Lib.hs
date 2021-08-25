import Data.List 

divisors :: Int -> [Int]
divisors n = [d | d <- [1..(abs n)], n `rem` d == 0]

prime :: Int -> Bool
prime n = divisors n == [1, n]

primeNumbers :: Int -> [Int]
primeNumbers n = [i | i <- [2..(abs n)], prime i]

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 l1 l2 l3 = zipAux (zip l1 l2) l3
    where
        zipAux [] _ = []
        zipAux _ [] = []
        zipAux ((x, y) : xs) (z : zs) = (x, y, z) : zipAux xs zs

-- --------------------------------------------------------
---------- HIGHER ORDER FUNCTIONS ----------------------
--------------------------------------------------------
apply2 :: (a -> a) -> a -> a
apply2 f x = f (f x)
--apply2 f = f.f
--apply2 f = \x -> f (f x)
-- apply2  = \f x -> f (f x)

firstEl :: [(a, b)] -> [a]
firstEl = map fst
-- firstEl [ ('a', 3), ('b', 2), ('c', 1)]

sumList :: [[Integer]] -> [Integer]
sumList = map sum 
-- sumList [[1, 3],[2, 4, 5], [], [1, 3, 5, 6]]

process2 :: [Integer] -> [Integer]
process2 = map processAux
    where
        processAux x 
            | even x = x `div` 2
            | otherwise = x * 2
-- process2 [2,4,5,6]

stringFilter :: Char -> [String] -> [String]
stringFilter ch = filter (ch `elem`)
-- stringFilter 'k' ["taxi", "computer", "haskell", "keyboard", "programming"]

oddSquares :: [Integer] -> [Integer]
oddSquares l = map (^2) (filter odd l)
-- oddSquares [1,2,3,4,5]

oddPosSquares :: [Integer] -> [Integer]
oddPosSquares l = map ((^2) . fst ) (filter (odd . snd) (zip l [1..]))

onlyVowels :: [String] -> [String]
onlyVowels = map removeConsonants
    where
        removeConsonants str = filter (`elem` "aeiouAEIOU") str

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x : xs) = f x : mymap f xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter p (x : xs) 
    | p x       = x : myfilter p xs
    | otherwise = myfilter p xs

primeNumbersSieve :: Int -> [Int]
primeNumbersSieve n = sieve [2..n]
    where
        sieve [] = []
        sieve (x:xs) = x : sieve [y | y <- xs, y `rem` x > 0]

naturalSort :: [Int] -> Bool
naturalSort [] = True
naturalSort [_] = True
naturalSort (_:xs) = and [x <= y | (x, y) <- zip xs (tail xs)]

naturalSort1 :: [Int] -> Bool
naturalSort1 [] = True
naturalSort1 [_] = True
naturalSort1 [x, y] = x <= y
naturalSort1 (x:xs)
    | x > head xs = False
    | otherwise = naturalSort1 xs

sorted :: [a] -> (a -> a -> Bool) -> Bool
sorted [] _ = True
sorted [x] _ = True
sorted [x, y] p = p x y
sorted (x:xs) p 
    | p x (head xs) = sorted xs p
    | otherwise = False

(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
x *<* y = (fst x < fst y) && (snd x < snd y)

test :: Bool
test = sorted [(1,2), (6,7), (9,10)] (*<*)

compositionList :: (b -> c) -> [a -> b] -> [a -> c]
compositionList f xs = [f . fs | fs <- xs]

applicationList :: a -> [a -> b] -> [b]
applicationList x fs = [f x | f <- fs]

test2 :: [Double]
test2 = applicationList 9 (compositionList (+1) [sqrt, (^2), (/2)])

myzip3' :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3' l1 l2 l3 = map unpack (zip (zip l1 l2) l3)
    where
        unpack :: ((Int, Int), Int) -> (Int, Int, Int)
        unpack ((x, y), z) = (x, y, z)
