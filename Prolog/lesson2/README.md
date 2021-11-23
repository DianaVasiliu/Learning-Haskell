# Lesson 2

In this lesson, we will learn how Prolog implements arithmetics, recursion and lists.

# Arithmetics

Example:

```prolog
?- 3+5 = +(3,5).
true

?- 3+5 = +(5,3).
false

?- 3+5 = 8.
false
```

Explanations:

-   `3+5` is a term
-   Prolog must be told explicitly to evaluate it as an arithmetic expression, using predefined predicates in Prolog, such as `is/2`, `=:=/2`, `>/2` etc.

### Exercise

Analyze the following examples:

```prolog
?- 3+5 is 8.
false

?- X is 3+5.
X = 8

?- 8 is 3+X.
is/2: Arguments are not sufficiently instantiated

?- X=4, 8 is 3+X.
false

?- X is 30-4.
X = 26

?- X is 3*5.
X = 15

?- X is 9/4.
X = 2.25
```

## The `is` operator

-   Takes two arguments
-   The second argument must be a valid arithmetic expression, with all the variables being instantiated
-   The first argument is either a number or a variable
-   If the first argument is a number, then the result is true if it's equal to the evaluation of the second argument expression
-   If the first argument is a variable, then the result is true if the variable can be unified with the evaluation of the second argument expression

However, it's not recommended to use `is` to compare two arithmetic expressions, but the `=:=` operator.

### Exercise

Analyze the following examples:

```prolog
?- 8 > 3.
true

?- 8+2 > 9-2.
true

?- 8 < 3.
false

?- 8 >= 3.
true

?- 8 =:= 3.
false

?- 8 =\= 3.
true
```

## Operators

In Prolog, there are two types of predefined operators:

-   functions
-   relations

## Functions

Addition and multiplication are examples of arithmetic functions.

Example:

```prolog
2 + (-3.2 * X - max(17,X)) / 2 ** 5
```

where `2**5` means `2^5`.

Examples of other available functions: `min/2`, `abs/1`, `sqrt/1`, `sin/1`.

The `//` operator is used for integer division.

The `mod` operator is used for the modulo operation.

## Relations

Arithmetic relations are used to compare the evaluation of arithmetic expressions (eg: `X > Y`).

Examples of available relations: `<`, `>`, `=<`, `>=`, `=\=` (not equal), `=:=` (arithmetic equality)

**Note**: be careful at the difference between `=:=` and `=`:

-   `=:=` compares two arithmetic expressions
-   `=` searches for an unifier

Example:

```prolog
?- 2 ** 3 =:= 3 + 5.
true

?- 2 ** 3 = 3 + 5.
false
```

# Recursion

In the last lesson, we used the following knowledge base:

```prolog
parent_of(rickardStark, eddardStark).
parent_of(rickardStark, lyannaStark).
parent_of(lyarraStark, eddardStark).
parent_of(lyarraStark, lyannaStark).

parent_of(aerysTargaryen, rhaegarTargaryen).
parent_of(rhaellaTargaryen, rhaegarTargaryen).

parent_of(rhaegarTargaryen, jonSnow).
parent_of(lyannaStark, jonSnow).
```

## Ancestors

We defined a predicate `ancestor_of(X, Y)`, which is true if `X` is an ancestor of `Y`.

The recursive function of this predicate is:

```prolog
ancestor_of(X, Y) :- parent_of(X, Y).
ancestor_of(X, Y) :- parent_of(X, Z), ancestor_of(Z, Y).
```

# Writing in Prolog

The predicate for writing in Prolog is `write/1`.

The predicate `nl/0` prints an empty line.

Example:

```prolog
?- write('Hello World!'), nl.
Hello World!
true

?- X = hello, write(X), nl.
hello
X = hello
```

# Lists

In Prolog, lists are some special data type (special terms).

Lists are written between square brackets, having the elements separated by comma.

`[]` is an empty list.

Examples:

```prolog
[elephant, horse, donkey, dog]
[elephant, [], X, parent(X, tom), [a, b, c], f(22)]
```

## Head & Tail

The first element in the list is called `Head`, and the rest of the list is called `Tail`.

Of course, an empty list does not have a `Head`.

In Prolog, there is an useful notation for lists, with the `|` separator, highlighting the first element and the rest of the list.

Examples:

```prolog
?- [1, 2, 3, 4, 5] = [Head | Tail].
Head = 1
Tail = [2, 3, 4, 5]

% with this notation, we can easily return, for example, the second element in a list
?- [quod, licet, jovi, non, licet, bovi] = [_, X | _].
X = licet
```

Example: `elements_of/2` predicate

-   it's a predicate that verifies if a list contains a specified element
-   `element_of(X, Y)` must return true if `X` is an element of `Y`

```prolog
/* If the first element of the list is the element we are searching for,
then we are done searching
*/
element_of(X, [X | _]).

% Otherwise, we verify that the element is in the rest of the list
element_of(X, [_ | Tail]) :- element_of(X, Tail).
?- element_of(a,[a,b,c]).
?- element_of(X,[a,b,c]).
```

Example: `concat_lists/3`

-   it's a predicate that is used to concatenate two lists
-   the third argument is the concatenation of the two lists

```prolog
concat_lists([], List, List).
concat_lists([Elem | List1], List2, [Elem | List3]) :-
        concat_lists(List1, List2, List3).

?- concat lists([1, 2, 3], [d, e, f, g], X).
?- concat lists(X, Y, [a, b, c, d]).
```

## Other predefined predicates

In Prolog, there are some predefined predicates for working with lists. For example:

- `length/2` - the second argument retursn the length of the first argument list
- `member/2` - returns true if the first argument is found in the second argument list
- `append/3` - identical to the `concat_list/3` predicate
- `last/2` - returns true if the second argument is equal to the last element of the list in the first argument
- `reverse/2` - the list in the second argument is the list in the first argument, but mirrored

