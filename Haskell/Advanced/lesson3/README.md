# Lesson 3

## `Maybe` monad

For this chapter, we will work in the `mMaybe.hs`, which contains the definition of the `Maybe` monad. The definition is commented out because the monad is already defined in `GHC.Base`.

0. Understand the monad operations `(>>=)` and `return`.

    ```haskell
    Prelude> return 3 :: Maybe Int
    Just 3
    Prelude> (Just 3) >>= (\x -> if (x > 0) then Just (x*x) else Nothing)
    Just 9
    ```

    Sometimes, we will use the derivated operation `(>>)`.

    ```haskell
    ma >> mb = ma >>= \_ -> mb

    Prelude> (Just 3) >> Nothing
    Nothing
    Prelude> (Just 3) >> (Just 6)
    Just 6
    ```

1. Define the operator of the composition of the enriched functions.

    ```haskell
    (<=<) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
    f <=< g = (\ x -> g x >>= f)
    ```

    a) Create some examples to help you understand how this operator behaves.

    b) Define the property

    ```haskell
    assoc :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> (Int -> Maybe Int) -> Int -> Bool
    ```

    which, for 3 given functions, verifies the associativity of the `(<=<)` operator:

    ```
    h <=< (g <=< f) $ x = (h <=< g) <=< f $ x
    ```

    Verify the property for particular functions, using `QuickCheck`.

2. We define

    ```haskell
    pos :: Int -> Bool
    pos x = if (x>=0) then True else False

    foo :: Maybe Int -> Maybe Bool
    foo mx = mx >>= (\x -> Just (pos x))
    ```

    a) Understand what `foo` does.

    b) Remember the `do` notation. Rewrite the `foo` function using the `do` notation.

3. We want to define a function that adds two values of type `Maybe Int`.

    ```haskell
    addM :: Maybe Int -> Maybe Int -> Maybe Int
    addM mx my = undefined
    ```

    Example:

    ```haskell
    Prelude> addM (Just 4) (Just 3)
    Just 7
    Prelude> addM (Just 4) Nothing
    Nothing
    Prelude> addM Nothing Nothing
    Nothing
    ```

    a) Define the `addM` function using any method.

    b) Define the `addM` function using monadic operations and `do` notation.

    c) Write a test that verifies that the two functions above are equivalent and run it using `QuickCheck`.

### `do` notation and sequencing

4. Transform using the `do` notation the following functions:

    ```haskell
    cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

    prod f xs ys = [f x y | x <- xs, y<-ys]

    myGetLine :: IO String
    myGetLine = getChar >>= \x ->
                    if x == '\n'
                        then return []
                        else myGetLine >>= \xs -> return (x:xs)
    ```

5. Transform using the sequencing notation the following function:

    ```haskell
    prelNo noin = sqrt noin

    ioNumber = do
        noin <- readLn :: IO Float
        putStrLn $ "Start\n" ++ (show noin)
        let noout = prelNo noin
        putStrLn $ "Finish"
        print noout
    ```

## `Writer log` monad

For this chapter, we will work in the `mWriter.hs`.

1. The file contains a definition of the `Writer String` monad (a bit modified, to compile without extra options).

    ```haskell
    newtype WriterS a = Writer { runWriter :: (a, String) }
    ```

    a) Define the functions `logIncrement` and `logIncrement2` and test them.

    b) Define the function `logIncrementN`, that generalizes `logIncrement2`, as it follows:

    ```haskell
    logIncrementN :: Int -> Int -> WriterS Int
    logIncrement x n = undefined
    ```

    Example:

    ```haskell
    Prelude> runWriter $ logIncrementN 2 4
    (6,"increment:2\nincrement:3\nincrement:4\nincrement:5\n")
    ```

2. Change the `WriterS` monad definition so that it produces the list of the logged messages, and not their concatenation. To avoid possible confusion, work in the `mWriterL.hs` file. Define the `logIncrementN` function in this context.

    ```haskell
    newtype WriterLS a = Writer {runWriter :: (a, [String])}
    ```

    Example:

    ```haskell
    Prelude> runWriter $ logIncrementN 2 4
    (6,["increment:2","increment:3","increment:4","increment:5"])
    ```

### `map` function in monadic context

We will continue to work in the `mWriterL.hs` file.

3. Usually, the first argument of the `map` function is a function `f :: a -> b`, for example:

    ```haskell
    Prelude> map (\x -> if (x>=0) then True else False) [1,-2,3]
    [True,False,True]
    ```

    In monadic context, the `f` function is enriched, which means it returns a monadic value.

    ```haskell
    isPos :: Int -> WriterLS Bool
    isPos x = if (x>= 0) then (Writer (True, ["pos"])) else (Writer (False, ["neg"]))
    ```

    What happens if we apply `map`?

    ```haskell
    map isPos [1,2,3]
    ```

    We get an error message! The `map` function returned a list of monadic values, that can be printed out as it follows:

    ```haskell
    Prelude> map runWriter $ map isPos [1,-2,3]
    [(True,["pos"]),(False,["neg"]),(True,["pos"])]
    ```

    **Problem**: what do we do if we want the effects to be chained and the end result to be a list of results?

4. Define a function that behaves like `map`, but the final result is chaining the effects.

    ```haskell
    mapWriterLS :: (a -> WriterLS b) -> [a] -> WriterLS [b]
    ```

    Example:

    ```haskell
    Prelude> runWriter $ mapWriterLS isPos [1,-2,3]
    ([True,False,True],["pos","neg","pos"])
    ```

5. Define similar functions to `mapWriterLS` for the `WriterS` and `Maybe` functions from the previous exercises.

## `Reader` monad

For this chapter, we will work in the `mReader.hs`.

1. We define the following data type:

    ```haskell
    data Person = Person { name :: String, age :: Int }
    ```

    a) Define the following functions:

    ```haskell
    showPersonN :: Person -> String
    showPersonA :: Person -> String
    ```

    that show the person name and the person age nicely, as it follows:

    ```haskell
    Prelude> showPersonN $ Person "ada" 20
    "NAME:ada"
    Prelude> showPersonA $ Person "ada" 20
    "AGE:20"
    ```

    b) Combine the functions defined above and write a function

    ```haskell
    showPerson :: Person -> String
    ```

    that shows all information about a person nicely, as it follows:

    ```haskell
    Prelude> showPerson $ Person "ada" 20
    "(NAME:ada,AGE:20)"
    ```

    c) Using the `Reader` monad, define monadic alternatives for the previous functions.

    ```haskell
    mshowPersonN :: Reader Person String
    mshowPersonA :: Reader Person String
    mshowPerson :: Reader Person String
    ```

    Example:

    ```haskell
    Prelude> runReader mshowPersonN $ Person "ada" 20
    "NAME:ada"
    Prelude> runReader mshowPersonA $ Person "ada" 20
    "AGE:20"
    Prelude> runReader mshowPerson $ Person "ada" 20
    "(NAME:ada,AGE:20)"
    ```
