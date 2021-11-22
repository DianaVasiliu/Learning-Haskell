# Grade Booster

## Problem 1

Write a function that receives a string and returns the string obtained from the initial string by transforming the uppercase letters into lowercase letters and vice versa.

1. Use only recursion (without list comprehension or higher order functions)
2. Use only list comprehension
3. Use only higher order functions (map, filter, foldr)

Write 2 non-trivial tests for your functions.

## Problem 2

Let the following data type:

```haskell
data Pol = X Integer -- represents the X variable raised to a (>=0) power
         | S Integer -- scalar
         | Pol :+: Pol -- sum
         | Pol :*: Pol -- product
         deriving (Show)
```

Write a function that verifies that a polynome is in the following form:

    (S v1 :*: X n1) :+: ((S v2 :*: X n2) :+: ... )

with `n1 > n2 > ...` and `v1, v2, ... =/= 0` (the powers appear in descending order and the coefficients are zero).

## Problem 3

Let the following data types:

```haskell
type Digit = O | I | D  -- O =0, I=1, D=2
data NBase = NB  Digit Digit Digit   -- numbers written in base 3 using 3 bits
```

Make `NBase` an instance of the following class:

```haskell
class MyNum x where
    myfromInteger :: Integer -> NBase
    plus :: NBase NBase -> NBase
```

-   `myfromInteger` will be defined only for positive values, throws error if the number cannot be represented on the specified number of bits
-   `plus` represents the sum in the given base
