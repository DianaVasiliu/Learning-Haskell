# Lesson 13 - I/O

```haskell
import Data.Char
```

We will start by exercising the input/output operations.

## Examples

1. Reading a string from the keyboard and outputing the result after processing.

    ```haskell
    procStr strin = map toUpper strin

    ioString = do
        strin <- getLine
        putStrLn $ "Input:\n" ++ strin
        let strout = procStr strin
        putStrLn $ "Output:\n" ++ strout
    ```

2. Reading a number from the keyboard and outputing the result after processing.

    ```haskell
    procNo noin = sqrt noin

    ioNumber = do
        noin <- readLn :: IO Double
        putStrLn $ "Input:\n" ++ (show noin)
        let noout = procNo noin
        putStrLn $ "Output:"
        print noout
    ```

3. Reading from a file and outputing the result in an output file.

    ```haskell
    inoutFile = do
        sin <- readFile "Input.txt"
        putStrLn $ "Input:\n" ++ sin
        let sout = procStr sin
        putStrLn $ "Output:\n" ++ sout
        writeFile "Output.txt" sout
    ```

## Exercises

1. Write a program that reads a number `n` from the keyboard and a sequence of `n` persons, for each reading the name and the age. The program must print the person (or persons) with the highest age. We assume that the age is an `Int`.

    Input example:

    ```
    3
    John Jonathan Jones
    70
    John Doe
    99
    Mustafa ben Muhamad
    7
    ```

    Output example:

    ```
    The oldest person is John Doe (99 years old).
    ```

2. Same as above, but the data is read from an input file, where each row contains information about a single person, the name and the age being separated by comma.

    Hint: you can use the `splitOn` function in the `Data.List.Split` module to separate the person information

## Our own I/O Monad

An input-output action that produces a result of type `a` can be modeled as a function from an input string to a tuple consisting of an element of type `a` representing the result, a string representing the part of the input string that has not been consumed and a string representing what the action displays.

```haskell
type Input = String
type Output = String

newtype MyIO a = MyIO { runIO :: Input -> (a, Input, Output)}
```

## Exercises

1.  Define the basic functions `myGetChar` and `myPutChar`.

    ```haskell
    myGetChar :: MyIO Char
    myGetChar = undefined

    testMyGetChar :: Bool
    testMyGetChar = runIO myGetChar "Anne" == ('A', "nne", "")

    myPutChar :: Char -> MyIO ()
    myPutChar = undefined

    testMyPutChar :: Bool
    testMyPutChar = runIO (myPutChar 'C') "Anne" == ((), "Anne", "C")
    ```

2.  Make `MyIO` an instance of `Functor`.

    ```haskell
    instance Functor MyIO where

    testFunctorMyIO :: Bool
    testFunctorMyIO = runIO (fmap toUpper myGetChar) "anne" == ('A', "nne", "")
    ```

3.  Make `MyIO` an instance of `Applicative`.

    Note: when we propagate the side effects, the input string is consumed, and the output strings are concatenated.

    ```haskell
    instance Applicative MyIO where

    testPureMyIO :: Bool
    testPureMyIO = runIO (pure 'C') "Anne" == ('C', "Anne", "")

    testApMyIO :: Bool
    testApMyIO = runIO (pure (<) <*> myGetChar <*> myGetChar) "Anne" == (True, "e", "")
    ```

4.  Make `MyIO` an instance of `Monad`.

    ```haskell
    instance Monad MyIO where

    testBindMyIO :: Bool
    testBindMyIO = runIO (myGetChar >>= myPutChar) "Anne" == ((), "nne", "A")
    ```
