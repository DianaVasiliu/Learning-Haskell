import Data.Maybe

{- the Maybe monad is defined in GHC.Base 

instance Monad Maybe where
    return = Just
    Just va  >>= k   = k va
    Nothing >>= _   = Nothing


instance Applicative Maybe where
    pure = return
    mf <*> ma = do
        f <- mf
        va <- ma
        return (f va)       

instance Functor Maybe where              
    fmap f ma = pure f <*> ma   
-}

-- 1. 
(<=<) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
f <=< g = (\ x -> g x >>= f)

-- 1. a)
f1 :: String -> Maybe Int
f1 ""  = Nothing
f1 str = Just (length str)

g1 :: String -> Maybe String
g1 "" = Nothing
g1 str = Just (str ++ str)

-- first, g1 is called, then it returns the result of f1
ex1 :: String -> Maybe Int
ex1 = f1 <=< g1

f2 :: String -> Maybe Bool
f2 s
    | s == "Even" = Just True 
    | s == "Odd"  = Just False 
    | otherwise  = Nothing

g2 :: Int -> Maybe String
g2 x 
    | x > 0 = if even x 
                then Just "Even" 
                else Just "Odd"
    | otherwise = Nothing 

ex2 :: Maybe Bool
ex2 = f2 <=< g2 $ 34

ex3 :: Maybe Bool
ex3 = f2 <=< g2 $ -4

ex4 :: Int -> Maybe Bool
ex4 = f2 <=< g2

-- 1. b)
assoc :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> (Int -> Maybe Int) -> Int -> Bool
assoc f g h x = ((h <=< g) <=< f $ x) == (h <=< (g <=< f) $ x)

f :: Int -> Maybe Int
f x 
    | x < 0 = Nothing
    | otherwise = Just x

g :: Int -> Maybe Int
g x 
    | even x = Just (x `div` 2)
    | odd x = Nothing

h :: Int -> Maybe Int
h x 
    | x `rem` 5 == 0    = Just (x `div` 5)
    | otherwise         = Nothing

test = assoc f g h
-- import Test.QuickCheck
-- quickCheck test


-- 2.
pos :: Int -> Bool
pos x = x>=0

foo :: Maybe Int ->  Maybe Bool 
foo mx = mx >>= \x -> Just (pos x)

-- 2. b)
foo' :: Maybe Int -> Maybe Bool
foo' mx = do
    x <- mx
    Just (pos x)

test1 = foo' (Just 3)
test2 = foo' (Just (-3))

-- 3. 
-- 3.1
addM :: Maybe Int -> Maybe Int -> Maybe Int  
addM (Just n1) (Just n2) = Just (n1 + n2)
addM _ _ = Nothing

-- 3.2
addM' :: Maybe Int -> Maybe Int -> Maybe Int
addM' mx my = do
    x <- mx
    y <- my
    return (x + y)

-- 3.3
test3 :: Maybe Int -> Maybe Int -> Bool
test3 x y = addM x y == addM' x y
-- quickCheck test3

-- 4.
cartesianProduct xs ys = xs >>= (\x -> ys >>= \y -> return (x, y))

cartesianProduct' xs ys = do
    x <- xs
    y <- ys
    return (x, y)

prod f xs ys = [f x y | x <- xs, y <- ys]
prod' f xs ys = xs >>= (\x -> ys >>= \y -> return (f x y))
prod'' f xs ys = do
    x <- xs
    y <- ys
    return (f x y)

myGetLine :: IO String
myGetLine = getChar >>= \x ->
                if x == '\n'
                    then return []
                    else myGetLine >>= \xs -> return (x:xs)
myGetLine' = do
    x <- getChar
    if x == '\n'
        then return []
        else do 
            xs <- myGetLine'
            return (x : xs)

-- 5.
prelNo = sqrt 
ioNumber = do 
    noin <- readLn ::IO Float
    putStrLn $ "Start\n" ++ show noin
    let noout = prelNo noin
    putStrLn "Finish" 
    print noout

ioNumber' = (readLn :: IO Float) >>= 
    \noin ->  putStrLn ("Start\n" ++ show noin)
            >> let noout = prelNo noin
                in (putStrLn "Finish" >> print noout)

-- 5.
isPos :: Int -> Maybe Bool
isPos x = if x >= 0 
            then Just True
            else Just False

mapMb :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMb _ [] = Nothing
mapMb f xs = 
    Just [fromJust $ f x | x <- xs]

test4 = mapMb isPos [1,-2,3]
test5 = map isPos [1,-2,3]
