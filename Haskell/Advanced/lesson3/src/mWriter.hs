import Data.Maybe

--- Writer monad

newtype WriterS a = Writer { runWriter :: (a, String) } 


instance Monad WriterS where
    return va = Writer (va, "")
    ma >>= k = let  (va, log1) = runWriter ma
                    (vb, log2) = runWriter (k va)
                in  Writer (vb, log1 ++ log2)

instance Applicative WriterS where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)       

instance Functor WriterS where              
    fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)

-- 1.
logIncrement :: Int -> WriterS Int
logIncrement x = do
    tell ("increment: " ++ show x ++ "\n")
    return (x + 1)

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
    y <- logIncrement x
    logIncrement y

test1 = runWriter $ logIncrement 12
test2 = runWriter $ logIncrement2 20

-- 1.2
logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n = do
    if n /= 0 
        then do
            y <- logIncrement x
            logIncrementN y (n - 1)
        else
            return x

test3 = runWriter $ logIncrementN 2 4

-- 5
isPos :: Int -> WriterS Bool
isPos x = if x>= 0 
            then Writer (True, "pos")
            else Writer (False, "neg")

mapWriterS :: (a -> WriterS b) -> [a] -> WriterS [b]
mapWriterS f xs = 
    let lst = [runWriter $ f x | x <- xs]
    in Writer ([fst pair | pair <- lst], concat [snd pair ++ "\n" | pair <- lst])

test4 = runWriter $ mapWriterS isPos [1,-2,3]
