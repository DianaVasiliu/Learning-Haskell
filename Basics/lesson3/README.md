# Lesson 3 - Lists, Higher Order Functions

Let's remember how to define lists using list comprehension from the last lesson. Try to find the values of the following expressions and verify your answers in the interpreter:

```haskell
[x^2 | x <- [1 .. 10], x `rem` 3 == 2]
[(x, y) | x <- [1 .. 5], y <- [x .. (x+2)]]
[(x, y) | x <- [1 .. 3], let k = x^2, y <- [1 .. k]]
[x | x <- "Learning Haskell - Lesson 3", elem x ['A' .. 'Z']]
[[x .. y] | x <- [1 .. 5], y <- [1 .. 5], x < y ]
```

## Exercises

1. Using list comprehension, define a function `divisors :: Int -> [Int]` that takes a number `n` as parameter and returns a list of all positive divisors of `n`.

2. Using the `divisors` function, define the predicate `prime n` that returns `True` if and only if `n` is a prime number.

3. Using list comprehension and the functions defined above, define a new function named `primeNumbers :: Int -> [Int]` that takes as parameter a number `n` and returns the list of prime numbers between `2` and `n`.

## The `zip` function

Test the following expressions and analyze the differences:

```haskell
Prelude> [(x, y) | x <- [1..5], y <- [1..3]]
Prelude> zip [1..5] [1..3]
```

## Exercise

4. Define a function named `myzip3` that has the same behavior as `zip`, but has 3 arguments:

    ```haskell
    *Main> myzip3 [1,2,3] [1,2] [1,2,3,4]
    [(1,1,1), (2,2,2)]
    ```

## Higher Order Functions

In Haskell, the functions are values. We can send them as parameters to another functions and we can return them from a function.

Let's assume we have a function named `apply2` that takes as parameter a function `f :: a -> a` and a value `x` of type `a` and returns the value of `f (f x)`. The `apply2` type is `apply2 :: (a -> a) -> a -> a`.

For this function, there are multiple definitions:

```haskell
apply2 f x = f (f x)
apply2 f = f . f
apply2 = \f x -> f (f x)
apply2 f = \x -> f (f x)
```

## The `map` function

The `map` function takes two arguments as arguments, a function `a -> b` and a list `[a]` and returns a list `[b]` obtained by applying the function to each element of the argument list.

```haskell
map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]
```

Examples:

```haskell
Prelude> map (* 3) [1, 3, 4]
[3, 9, 12]
Prelude> map ($ 3) [(4 +), (10 *), (^ 2), sqrt]
[7.0, 30.0, 9.0, 1.7320508075688772]
```

Try to find the values of the following expressions and verify them in the interpreter:

```haskell
map (\x -> 2 * x) [1..10]
map (1 `elem`) [[2, 3], [1, 2]]
map (`elem` [2, 3]) [1, 3, 4, 5]
```

## Exercises

Solve the following exercises using the `map` function.

5. Write a function that takes as argument a list of pairs `(a, b)` and returns the list of the first elements of the pairs.

    Example:

    ```haskell
    Prelude> firstEl [('a', 3), ('b', 2), ('c', 1)]
    "abc"
    ```

6. Write the function `sumList` that takes as parameter a list of lists of `Integer` elements and returns the list of sums of each inner list.

    Example:

    ```haskell
    Prelude> sumList [[1, 3],[2, 4, 5], [], [1, 3, 5, 6]]
    [4, 11, 0, 15]
    ```

7. Write a function named `process2` that takes as argument a list of `Integer` values and returns a list where the even elements are divided by 2 and the odd numbers are doubled.

    Example:

    ```haskell
    *Main> process2 [2, 4, 5, 6]
    [1, 2, 10, 3]
    ```

## The `filter` function

The `filter` function takes as arguments a property `a -> Bool` and a list of elements `[a]`, returning the list of elements that verify the given property.

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]
```

Examples:

```haskell
Prelude> filter (>2) [3, 1, 4, 2, 5]
[3, 4, 5]
Prelide> filter odd [3, 1, 4, 2, 5]
[3, 1, 5]
```

## Exercises

Solve these exercises using `map` and `filter`, but without using recursion.

8. Write a function that takes as arguments a character and a list of strings and returns a list of the strings that contain the specified character (use the `elem` function).

9. Write a function that takes as argument a list of `Integer` values and returns a list of the squares of the odd numbers.

10. Write a function that takes as argument a list of `Integer` values and returns a list of the squares of the numbers in odd positions. To access the position, use the `zip` function.

11. Write a function that takes as argument a list of strings and returns the list of strings after all the consonants have been removed.

    Example:

    ```haskell
    *Main> onlyVowels ["Amazing", "Functional", "Programming", "Course"]
    ["Aai", "uioa", "oai", "oue"]
    ```

## `mymap`, `myfilter` functions

Define the functions `mymap` and `myfilter` recursively having the same behavior as the `map` and `filter` functions.

## Sieve of Eratosthenes

Define a function `primeNumbersSieve :: Int -> [Int]` that implements the Sieve of Eratosthenes algorithm.

## Exercises

12. Using the list comprehension and the `and` and `zip` functions, complete the definition of the function `naturalSort` that verifies if a list of `Int` numbers is sorted, using the natural sorting order.

```haskell
naturalSort :: [Int] -> Bool
naturalSort [] = True
naturalSort [x] = True
naturalSort (x:xs) = undefined
```

13. Without using comprehension, but using recursion, define the function `naturalSort1` that has the same behavior as `naturalSort`.

14. Write a function `sorted :: [a] -> (a -> a -> Bool) -> Bool` that takes as parameter a list and a binary relationship and returns `True` of any consecutive values are in the given relationship.

    a) Define the function using any method.

    b) Check the definition in the interpreter for different values:

    -   integer numbers with the order relationship
    -   integer numbers with the divisibility relationship
    -   strings with lexicographic relationship

    c) Read about the [operators](https://wiki.haskell.org/Section_of_an_infix_operator) in Haskell. Define a new operator `(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool` that defines an order relationship on pairs. Using the `sorted` function, check if a list of pairs is sorted by the `*<*` relationship.

15. Write a function `compositionList :: (b -> c) -> [(a -> b)] -> [(a -> c)]` that takes as parameter a function and a list of functions and returns a list of functions obtained by composing the argument function with each of the functions in the list.

    Write a function `applicationList :: a -> [(a -> b)] -> [b]` that takes as parameter an element of type `a` and a list of functions ald returns the list of values obtained by applying each function to the element.

    Example:

    ```haskell
    *Main> applicationList 9 [sqrt, (^2), (/2)]
    [3.0, 81.0, 4.5]
    ```

    Now we can test the `compositionList` function.

    Example:

    ```haskell
    *Main> applicationList 9 (compositionList (+1) [sqrt, (^2), (/2)])
    [4.0, 82.0, 5.5]
    ```

16. Write the `myzip3` function using only `map` and `zip` functions.
