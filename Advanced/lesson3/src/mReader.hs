newtype Reader env a = Reader { runReader :: env -> a }

instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env

instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor (Reader env) where
  fmap f ma = pure f <*> ma

ask :: Reader env env
ask = Reader id

local :: (r -> r) -> Reader r a -> Reader r a
local f ma = Reader $ (\r -> (runReader ma)(f r))

-- 1.
data Person = Person { name :: String, age :: Int }

-- 1. a)
showPersonN :: Person -> String
showPersonN (Person name age) = "NAME:" ++ name

showPersonA :: Person -> String
showPersonA (Person name age) = "AGE:" ++ show age

test1 = showPersonN $ Person "ada" 20
test2 = showPersonA $ Person "ada" 20

-- 1. b)
showPerson :: Person -> String
showPerson person = "(" ++ showPersonN person ++ "," ++ showPersonA person ++ ")"

test3 = showPerson $ Person "ada" 20

-- 1. c)
mshowPersonN ::  Reader Person String
mshowPersonN = Reader showPersonN

mshowPersonA ::  Reader Person String
mshowPersonA = Reader showPersonA

mshowPerson :: Reader Person String
mshowPerson = Reader showPerson

test4 = runReader mshowPersonN $ Person "ada" 20
test5 = runReader mshowPersonA $ Person "ada" 20
test6 = runReader mshowPerson  $ Person "ada" 20
