# Midterm

## Problem 1

Consider the score of a word, calculated as follows:

-   a character has the score 10 if it's found in the 'Haskell' word, ignoring the capital letters
-   otherwise, it has score 1
-   the score of the word is the product of it's letters scores
-   the empty string has score 1

Write a function that takes a string as argument and returns its score.

1. Use only recursion (without list comprehension or higher order functions). Name the function `scoreProductRec`.
2. Use only list comprehension. Name the function `scoreProductComp`.
3. Use only higher order functions (map, filter, foldr). Name the function `scoreProductFunc`.

Write a `quickCheck` test that checks if the `scoreProductComp` and `scoreProductFunc` are equivalent.

## Problem 2

Write a function that takes a list of comparable elements and returns the number of elements that occur at least twice in the list.

Example:

```haskell
> f [1, 2, 3, 2, 4, 1, 5, 3, 4, 2, 3, 1, 6]
4
```
