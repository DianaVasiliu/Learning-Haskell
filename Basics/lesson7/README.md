# Lesson 7 - QuickCheck

QuickCheck is a library used for testing the code, generating unit tests. To use the library, the module must be included at the top of the file, using `import Test.QuickCheck`.

## Exercises

1. Define the following functions, that take a number as argument and multiplies it by 2, 3 or 5 respectively.

    ```haskell
    double :: Int -> Int
    double = undefined

    triple :: Int -> Int
    triple = undefined

    penta :: Int -> Int
    penta = undefined
    ```

2. Analyze the following test. What is the type of `test`? Run the command `quickCheck test` in the interpreter and analyze the result.

    ```haskell
    test x = (double x + triple x) == penta x
    ```

3. Write a test that verifies a false expression and analyze the result.

4. Write a function that searches for an integer element in a list of key-value pairs and returns the value, using a `Maybe String` result.

    Write a test that verifies that the function has the same results as the predefined `lookup` function.

    ```haskell
    myLookUp :: Int -> [(Int,String)]-> Maybe String
    myLookUp = undefined

    testLookUp :: Int -> [(Int,String)] -> Bool
    testLookUp = undefined
    ```

## Using QuickCheck with constraints

Let's try testing if `myLookUp` is equivalent with `lookup` only for positive keys that are divisible by 5.

```haskell
testLookUpCond :: Int -> [(Int,String)] -> Property
testLookUpCond n list = n > 0 && n `div` 5 == 0 ==> testLookUp n list
```

We can see that the `testLookUpCond` has a return type of `Property`. The constraint in the left side of the `==>` operator selects only the input data that satisfies the constraint.

Test `quickCheck testLookUpCond` in the interpreter and see the result.

## Exercise

5. a) Write a function `myLookUp'`, with the same signature as `myLookUp`, that capitalizes the first letter of the value, if the key is found.

    b) Write a predicate that test wether if `myLookUp` is equivalent to `lookup` for the lists that contain only values that start with capitalized letter. Test it using `quickCheck`.

## Testing the abstract data types

## Exercises

6. Define an instance of the `Arbitrary` class for the `ElemIS` data type.

    ```haskell
    data ElemIS = I Int | S String
        deriving (Show, Eq)
    ```

7. Write a function `myLookUpElem`, which is similar to `lookup`, except that it searches through a list of pairs where the key is `Int` and the value is `ElemIS`.

    ```haskell
    myLookUpElem :: Int -> [(Int,ElemIS)]-> Maybe ElemIS
    myLookUpElem = undefined
    ```

    Write a test and run `quickCheck testLookUpElem` in the console.

    ```haskell
    testLookUpElem :: Int -> [(Int,ElemIS)] -> Bool
    testLookUpElem = undefined
    ```
