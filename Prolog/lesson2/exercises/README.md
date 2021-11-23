# Exercises

## Exercise 1

Define a predicate `distance/3` that calculates the distance between two points in a 2D plane. The points are given as coordinate pairs.

Example:

```prolog
?- distance((0,0), (3,4), X).
X = 5.0

?- distance((-2.5,1), (3.5,-4), X).
X = 7.810249675906654
```

## Exercise 2

Write a predicate `fib/2` that calculates the n-th Fibonacci number.

The Fibonacci sequence is defined as follows:

F <sub>0</sub> = 1

F <sub>1</sub> = 1

F <sub>n</sub> = F <sub>n-1</sub> + F <sub>n-2</sub>, for n >= 2

Examples:

```prolog
?- fib(1,X).
X=1.
true

?- fib(2,X).
X=2.
true

?- fib(5,X).
X=8.
true
```

The program answers the question `?-fib(50, X).`?

If yes, then congratulations! Otherwise, try to find a better solution.

_Hint_: Try to build all the Fibonacci numbers until you find the desired number.

## Exercise 3

Write a Prolog program that prints on the screen a `n x n` square of characters.

Name the predicate `square/2`. The first argument is a natural number, different from 0, and the second argument is a character (i.e. any term in Prolog), that must be printed.

Example:

```prolog
?- square(5, '* ').
* * * * *
* * * * *
* * * * *
* * * * *
* * * * *
```

## Exercise 4

a) Define a predicate `all_a/1` that takes as argument a list and verifies that all the elements of the list are just `a`'s.

Example:

```prolog
?- all a([a,a,a,a]).
?- all a([a,a,A,a]).
```

b) Write a predicate `trans_a_b/2` that translates a list of `a`'s into a list of `b`'s. `trans_a_b(X, Y)` must be true if the input `X` is a list of `a`'s and the output `Y` is a list of `b`'s, and both `X` and `Y` have the same length.

Example:

```prolog
?- trans a b([a,a,a], L).
?- trans a b([a,a,a], [b]).
?- trans a b(L, [b,b]).
```

## Exercise 5

a) Write a predicate `scalarMult/3`, whose first argument is an integer number, the second is a list of integers and the third is the result of their scalar multiplication.

Example:

```prolog
?-scalarMult(3, [2,7,4], Result).
Result = [6,21,12]
```

b) Write a predicate `dot/3`, whose first argument is a list of integers, the second one is a list of integers having the same length as the first list, and the third one is the result of the scalar multiplication of the elements in the two lists.

Example:

```prolog
?- dot([2,5,6], [3,4,1], Result).
Result = 32
```

c) Write a predicate `max/2` that searches for the maximum value in a list of natural numbers.

Example:

```prolog
?- max([4,2,6,8,1], Result).
Result = 8
```

## Exercise 6

Define a predicate `palindrome/1` that returns true if the list given as parameter is a palindrome (reading the list from left to right is the same as reading the list from right to left).

Example:

```prolog
?- palindrome([r,e,d,i,v,i,d,e,r]).
true
```

Don't use the `reverse` predefined predicate, but your own implementation of this predicate.

## Exercise 7

Define a predicate `remove_duplicates/2` that deletes all the duplicates in the list given as parameter and returns the result in the second argument.

Example:

```prolog
?- remove duplicates([a, b, a, c, d, d], List).
List = [b, a, c, d].
```
