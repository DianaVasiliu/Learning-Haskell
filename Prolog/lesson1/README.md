# Lesson 1

Prolog is the best known logic programming language.

-   based on first order classical logic (with predicates)
-   works on the basis of unification and search

Many implementations turn it into a "mature" programming language.

-   I/O, operations already implemented in language, etc.

We will be using the `SWI-Prolog` implementation.

-   it's free
-   contains a lot of libraries
-   https://www.swi-prolog.org/

The `SWISH` online version:

-   https://swish.swi-prolog.org/

## Atoms

-   sequences of letters, numbers and \_, starting with a lowercase letter
-   strings in apostrophes `'Atom'`
-   some special symbols

Examples:

-   `sansa`
-   `jon_snow`
-   `'Ser Gregor Clegane'`
-   `'(@*+'`
-   `+`

```prolog
?- atom('(@*+').
true.
```

`atom/1` is a predefined predicate.

## Constants

-   atoms: `sansa`, `'I am an atom'`
-   numbers: `2`, `2.5`, `-33`

## Variables

-   sequences of letters, numbers and _, starting with a capital letter or _
-   special variable: `_` is an **anonymous variable**

-   two appearances of `_` symbol are different variables. It is used when we don't want detailed information about the variable.

Examples:

-   `X`
-   `Arya`
-   `_cersei`

## Compound terms

-   their type is `p(t1, ..., tn)`, where
    -   `p` is an atom
    -   `t1, ..., tn` are terms

Examples:

-   `dislike(cersei, tyrion)`
-   `dislike(cersei, X)`

A compond term has:

-   a **name** (functor) - `dislike` in our example
-   an **arity** (number of arguments) - `2` in our example

# Example 1 - `kb1`

A Prolog program defines a knowledge base.

Example:

```prolog
stark(eddard).
stark(jon_snow).
stark(sansa).

lannister(tyrion).
lannister(cersei).

dislike(cersei, tyrion).
```

## Predicates

A knowledge base is a set of predicates that define the world (universe) of that program.

Example:

```prolog
stark(eddard).
stark(jon_snow).
stark(sansa).

lannister(tyrion).
lannister(cersei).

dislike(cersei, tyrion).
```

This program contains 3 predicates: `stark/1`, `lannister/1`, `dislike/2`.

### Defining the predicates

-   Predicates with the same name, but with different arities, are different predicates.

-   We write `foo/n` to indicate that a `foo` predicate has the arity `n`.

-   Predicates can have arity 0 (no arguments); they are predefined in language (`true`, `false`).

# Example 2 - `kb2`

A `rule` is a statement with the form `Head :- Body.`

A `fact` is a rule without `Body`.

Examples:

```prolog
eating(joffrey).
deceased(robert).
dislike(cersei, tyrion).

happy(cersei) :- happy(joffrey).
happy(ser_jamie) :- happy(cersei), deceased(robert).
happy(joffrey) :- dislike(joffrey, sansa).
happy(joffrey) :- eating(joffrey).
```

A `rule` is a statement with the form `Head :- Body.` where:

-   `Head` is a predicate (complex term)
-   `Body` is a predicate sequence, separated by comma

Example:

```prolog
happy(ser_jamie) :- happy(cersei), deceased(robert).
```

Interpreting:

-   `:-` is interpreted as implication (`<-`)
-   `,` is interpreted as conjunction (`∧`)

Thus, we can say that `happy(ser_jamie)` is equivalent to `happy(cersei) ∧ deceased(robert)`.

Multiple rules having the same `Head` are interpreted as they are separated by logical `or`.

Example:

```prolog
happy(joffrey) :- dislike(joffrey, sansa).
happy(joffrey) :- eating(joffrey).
```

If `dislike(joffrey, sansa)` is true or `eating(joffrey)` is true, then `happy(joffrey)` is true. Multiple rules with the same left side can be combined using `;`.

Example:

```prolog
happy(joffrey) :- dislike(joffrey, sansa); eating(joffrey).
```

Thus, we can say that `happy(joffrey)` is equivalent to `dislike(joffrey, sansa) ∨ eating(joffrey)`.

## The syntax

A Prolog program is a collection of facts and rules.

The facts and rules must be grouped by the `Head` atoms.

Example:

| Correct                  | Incorrect                |
| ------------------------ | ------------------------ |
| stark(eddard).           | stark(eddard).           |
| stark(jon_snow).         | dislike(cersei, tyrion). |
| stark(sansa).            | stark(sansa).            |
|                          |                          |
| lannister(tyrion).       | lannister(tyrion).       |
| lannister(cersei).       | stark(jon_snow).         |
|                          |                          |
| dislike(cersei, tyrion). | lannister(cersei).       |

## Queries

A `query` is a sequence with the form

`?- p1(t1, ..., tn), ..., pn(t1', ..., tn')`

Given a query, Prolog is looking for `answers`.

-   true / false, if the query does not contain variables
-   if the query contains variables, then Prolog is looking for values that make all the predicates in the query true; if there are no such values, the returned answear is false

The predicates that must be satisfiable to answer the query are called `goals`.

Examples:

```prolog
eating(joffrey).
deceased(rickard).
dislike(cersei, tyrion).

happy(cersei) :- happy(joffrey).
happy(ser_jamie) :- happy(cersei),
                    deceased(robert).
happy(joffrey) :- dislike(joffrey, sansa).
happy(joffrey) :- eating(joffrey).
```

```prolog
?- happy(joffrey).
true

?- happy(cersei).
true

?- happy(ser_jamie).
false

?- happy(X).
X = cersei ;
X = joffrey.
```

# Example 3 - `kb3`

Example:

```prolog
father(eddard, sansa).
father(eddard, jon_snow).

mother(catelyn, sansa).
mother(wylla, jon_snow).

stark(eddard).
stark(catelyn).

stark(X) :- father(Y, X),
            stark(Y).
```

For any `X`, `Y`, if `father(Y, X)` is true and `stark(Y)` is true, then `stark(X)` is true, i.e., for any `X`, if the `father` of `X` is `stark`, then `X` is `stark`.

Example:

```prolog
?- stark(jon_snow).
true

?- stark(X).
X = eddard
X = catelyn
X = sansa
X = jon_snow
false

?- stark(X), mother(Y,X), stark(Y).
X = sansa,
Y = catelyn
false
```

# Other information

-   a Prolog program is written in a file having the `.pl` extension
-   comments:
    -   `%` - comment on a single line
    -   `/* */` = comment on multiple lines
-   don't forget to write `.` at the end of a fact or a rule
-   a program is loaded using:
    -   `?- [name].`
    -   `?- ['<path>/name.pl'].`

# Negation

In Prolog, there is the predefined predicate `not` with the following semantics: `not(goal)` is true if `goal` cannot be proven in the current knowledge base.

**Note**: `not` **isn't** a logical negation; it expresses the impossibility of making the demonstration (or instantiation) according to the knowledge base ("closed world assumption"). To mark this distinction, in the new language variants, the `\+` operator can be used instead of `not`.

Example:

```prolog
not_parent(X, Y) :- not(parent_of(X, Y)).
not_parent(X, Y) :- \+ parent_of(X, Y).
```

# Sources

-   [Learn Prolog](http://www.let.rug.nl/bos/lpn//)
-   [Prolog programming: a do-it-yourself course for beginners](https://cs.union.edu/~striegnk/courses/esslli04prolog/)
