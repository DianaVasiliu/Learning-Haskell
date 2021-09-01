import Data.Char
import Data.List

procStr :: [Char] -> [Char]
procStr = map toUpper

ioString :: IO ()
ioString = do
    strin <- getLine
    putStrLn $ "Input:\n" ++ strin
    let strout = procStr strin
    putStrLn $ "Output\n" ++ strout

procNo :: Double -> Double
procNo = sqrt

ioNumber :: IO ()
ioNumber = do
    noin <- readLn :: IO Double
    putStrLn $ "Input:\n" ++ show noin
    let noout = procNo noin
    putStrLn "Output:"
    print noout

inoutFile :: IO ()
inoutFile = do
    sin <- readFile "Input.txt"
    putStrLn $ "Input:\n" ++ sin
    let sout = procStr sin
    putStrLn $ "Output:\n" ++ sout
    writeFile "Output.txt" sout

type Person = (String, Int)

readPerson :: IO Person
readPerson = do
    name <- getLine
    age <- readLn
    return (name, age)

showPerson :: Person -> String
showPerson (name, age) = name <> " (" <> show age <> " years old)"

showPersons :: [Person] -> String
showPersons [] = ""
showPersons [p] = "The oldest person is " <> showPerson p <> "."
showPersons ps =
    "The oldest persons are "
    <> intercalate ", " (map showPerson ps)
    <> "."

oldestPersons :: [Person] -> [Person]
oldestPersons ps = filter ((== m) . snd) ps
    where
        m = maximum (map snd ps)

ex1 :: IO ()
ex1 = do
    n <- readLn :: IO Int
    persons <- sequence (replicate n readPerson)
    let oldest = oldestPersons persons
    putStrLn (showPersons oldest)

readPersonComma :: String -> Person
readPersonComma s = (name, read age)
    where
        (name, ',' : ' ' : age) = break (== ',') s

readPersons :: String -> [Person]
readPersons = map readPersonComma . lines

ex2 :: IO ()
ex2 = do
    persons <- readPersons <$> readFile "ex2.in"
    let oldest = oldestPersons persons
    putStrLn (showPersons oldest)

-- ---------------------------------
-- --------------- MyIO ------------
-- ---------------------------------
type Input = String
type Output = String

newtype MyIO a = MyIO { runIO :: Input -> (a, Input, Output)}

myGetChar :: MyIO Char
myGetChar = MyIO (\(c : sin) -> (c, sin, ""))

testMyGetChar :: Bool
testMyGetChar = runIO myGetChar "Anne" == ('A', "nne", "")

myPutChar :: Char -> MyIO ()
myPutChar c = MyIO (\sin -> ((), sin, [c]))

testMyPutChar :: Bool
testMyPutChar = runIO (myPutChar 'C') "Anne" == ((), "Anne", "C")

instance Functor MyIO where
    fmap f ioa = MyIO iob
        where
            iob sin = (f a, sin', sout')
                where
                    (a, sin', sout') = runIO ioa sin

testFunctorMyIO :: Bool
testFunctorMyIO = runIO (fmap toUpper myGetChar) "anne" == ('A', "nne", "")

instance Applicative MyIO where
    pure a = MyIO (\sin -> (a, sin, ""))
    iof <*> ioa = MyIO iob
        where
            iob sin = (f a, sin'', sout' ++ sout'')
                where
                    (f, sin', sout') = runIO iof sin
                    (a, sin'', sout'') = runIO ioa sin'


testPureMyIO :: Bool
testPureMyIO = runIO (pure 'C') "Anne" == ('C', "Anne", "")

testApMyIO :: Bool
testApMyIO = runIO (pure (<) <*> myGetChar <*> myGetChar) "Anne" == (True, "ne", "")

testApMyIO' :: Bool
testApMyIO' = runIO (myGetChar <* myPutChar 'M' <* myPutChar 'e') "Anne" == ('A', "nne", "Me")

instance Monad MyIO where
    return = pure
    ioa >>= k = MyIO iob
        where
            iob sin = (b, sin'', sout' ++ sout'')
                where
                    (a, sin', sout') = runIO ioa sin
                    (b, sin'', sout'') = runIO (k a) sin'

testBindMyIO :: Bool
testBindMyIO = runIO (myGetChar >>= myPutChar) "Anne" == ((), "nne", "A")