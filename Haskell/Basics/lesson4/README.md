# Lesson 4 - `foldr` function, Lazy Evaluation, `foldr` universality

## `foldr` function

The `foldr` function is used for the agregation of a collection. One definition of this function can be:

`` foldr op unit [a1, a2, a3, ..., an] == a1 `op` a2 `op` a3 `op` ... `op` an `op` unit ``

## Exercises

1.  a) Write a recursive function that calculates the product of the elements of a list.

    ```haskell
    productRec :: [Integer] -> Integer
    productRec = undefined
    ```

    b) Write an equivalent function that uses `foldr` instead of recursion.

    ```haskell
    productFold :: [Integer] -> Integer
    productFold = undefined
    ```

2.  a) Write a recursive function that verifies that all the elements in a list are `True`.

    ```haskell
    andRec :: [Bool] -> Bool
    andRec = undefined
    ```

    b) Write an equivalent function that uses `foldr` instead of recursion.

    ```haskell
    andFold :: [Bool] -> Bool
    andFold = undefined
    ```

3.  a) Write a recursive function that concatenates a list of lists.

    ```haskell
    concatRec :: [[a]] -> [a]
    concatRec = undefined
    ```

    b) Write an equivalent function that uses `foldr` instead of recursion.

    ```haskell
    concatFold :: [[a]] -> [a]
    concatFold = undefined
    ```

4.  a) Write a function that removes a character from a string.

    ```haskell
    rmChar :: Char -> String -> String
    rmChar = undefined
    ```

    b) Write a recursive function that removes all characters from second argument that are found in the first argument.

    ```haskell
    rmCharsRec :: String -> String -> String
    rmCharsRec = undefined

    test_rmchars :: Bool
    test_rmchars = rmCharsRec ['a'..'l'] "football" == "oot"
    ```

    c) Write an equivalent function that uses `foldr` instead in recursion.

    ```haskell
    rmCharsFold :: String -> String -> String
    rmCharsFold = undefined
    ```

## Lazy Evaluation

Haskell is a lazy language. This means that:

1. The evaluation of an expression is postponed until it's necessary for the program to continue. This means that the arguments of a function are not evaluated before the function is called.

2. Even when the expression is evaluated, the evaluation is partial, just as it is necessary to unlock the execution of the program.

3. In order to avoid the evaluation of the same argument of a function every time it is used in the body of the function, all the occurrences of a variable are shared, the partial expansion of the evaluation being done for all simultaneously.

Further, we will use an intentionally inefficient implementation of a function to test these hypotheses. The `logistic` function simulates an evolution law and has been proposed as a random number generator.

```haskell
logistic :: Num a => a -> a -> Natural -> a
logistic rate start = f
    where
        f 0 = start
        f n = rate * f (n - 1) * (1 - f (n - 1))
```

To simplify the function, we will use a function `logistic0` in which `rate` and `start` have been instantiated.

```haskell
logistic0 :: Fractional a => Natural -> a
logistic0 = logistic 3.741 0.00079
```

## Exercises

For the next exercises, we will need an expression that takes a long time to evaluate, to see if it gets evaluated.

1.  Test that the `logistic0` evaluation takes exponential time with the given argument. Choose a value for `ex1` large enough to see if it gets evaluated or not.

    ```haskell
    ex1 :: Natural
    ex1 = undefined
    ```

2.  Which of the following expressions will need the `logistic0 ex1` expression to be evaluated? Test them in the interpreter.

    ```haskell
    ex20 :: Fractional a => [a]
    ex20 = [1, logistic0 ex1, 3]

    ex21 :: Fractional a => a
    ex21 = head ex20

    ex22 :: Fractional a => a
    ex22 = ex20 !! 2   -- lst !! pos -> returns the element on the pos position in the list


    ex23 :: Fractional a => [a]
    ex23 = drop 2 ex20 -- drop n list -> removes the first n elements in the list

    ex24 :: Fractional a => [a]
    ex24 = tail ex20
    ```

3.  We define the following auxiliary functions:

    ```haskell
    ex31 :: Natural -> Bool
    ex31 x = x < 7 || logistic0 (ex1 + x) > 2

    ex32 :: Natural -> Bool
    ex32 x = logistic0 (ex1 + x) > 2 || x < 7
    ```

    Which of the following expressions will need `logistic0 (ex1 + x)` to be evaluated? Test them in the interpreter.

    ```haskell
    ex33 :: Bool
    ex33 = ex31 5

    ex34 :: Bool
    ex34 = ex31 7

    ex35 :: Bool
    ex35 = ex32 5

    ex36 :: Bool
    ex36 = ex32 7
    ```

## `foldr` universality

One possible implementation of `foldr` is:

```haskell
foldr_ :: (a -> b -> b) -> b -> ([a] -> b)
foldr_ op unit = f
    where
        f [] = unit
        f (a : as) = a `op` f as
```

Knowing this implementation, we can think of what recursive functions can be defined using `foldr`. So, given a function `f :: [a] -> b` for which we can discover a `unit :: b` and `op :: a -> b -> b` so that `f [] = unit` and `f (a:as) = op a (f as)`, then `f = foldr op unit`.

Examples:

The sum of the squares of the odd elements

```haskell
oddSquaresSum :: [Integer] -> Integer
oddSquaresSum [] = 0
oddSquaresSum (a : as)
    | odd a = a * a + oddSquaresSum as
    | otherwise = oddSquaresSum as

-- the foldr version
oddSquaresSumFold :: [Integer] -> Integer
oddSquaresSumFold = foldr op unit
    where
        unit = 0
        a `op` suma
            | odd a = a * a + suma
            | otherwise = suma
```

The `map` function

```haskell
map_ :: (a -> b) -> [a] -> [b]
map_ f [] = []
map_ f (a : as) = f a : map_ f

-- the foldr version
mapFold :: (a -> b) -> [a] -> [b]
mapFold f = foldr op unit
    where
        unit = []
        a `op` l = f a : l
```

The `filter` function

```haskell
filter_ :: (a -> Bool) -> [a] -> [a]
filter_ p [] = []
filter_ p (a : as)
    | p a = a : filter_ p as
    | otherwise = filter_ p as

-- the foldr version
filterFold :: (a -> Bool) -> [a] -> [a]
filterFold p = foldr op unit
    where
        unit = []
        a `op` filtered
            | p a = a : filtered
            | otherwise = filtered
```

## Exercise

4. Using only recursion and basic functions, write a function `sign` that takes as parameter a list of `Integer` values and returns a string that contains the sign of the numbers between -9 and 9 inclusive, ignoring the rest.

    ```haskell
    sign :: [Integer] -> String
    sign = undefined

    test_sign :: Bool
    test_sign = sign [5, 10, -5, 0] == "+-0" -- 10 is ignored
    ```

    Following the algorithm described, rewrite the function using `foldr` instead of recursion.

    ```haskell
    signFold :: [Integer] -> String
    signFold = foldr op unit
        where
            unit = undefined
            op = undefined
    ```
