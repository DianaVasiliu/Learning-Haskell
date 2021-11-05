# Lesson 4

## Monadic interpretation of programs

Next, we will start exploring the use of monads for structuring functional programs.

We will start with a simple lambda calculator, a simplified version of the MicroHaskell interpreter.

## Abstract syntax

A term is a variable, a constant, a sum, a lambda function or a function application.

```haskell
type Name = String

data Term
    = Var Name
    | Con Integer
    | Term :+: Term
    | Lam Name Term
    | App Term Term
    deriving (Show)
```

The language is small for illustrative purposes. It can be easily extended with several values (such as booleans, pairs and lists) and several kinds of expressions, such as conditional expressions and fixed point operator (recursion).

We will use the following test expression:

```haskell
term0 = (App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11))
```

Using the conventional notation, this would be written as: `((\ x -> x + x) (10 + 11))`

The value of the evaluation of `term0` is `42`.

As part of this lab, a simple interpreter is modified to support various side effects defined by monads.

## Values

A value is `Wrong`, a number or a function. The `Wrong` value indicates an error such as an undefined variable, an attempt to add non-numeric values, or an attempt to apply a non-functional value.

```haskell
data Value
    = Num Integer
    | Fun (Value -> M Value)
    | Wrong

instance Show Value where
    show (Num x) = show x
    show (Fun _) = "<function>"
    show Wrong = "<wrong>"
```

# Monadic Variations

For each of the exercises (variations) below, copy the `var0Identity.hs` file as `varM.hs` and modify it according to the requirements of the exercise.

## Exercise: General Monadic Interpreter

We will start with the trivial monad, which has no side effects.

```haskell
newtype Identity a = Identity { runIdentity :: a }
```

-   Make `Identity` an instance of the `Show` class.

    `show` extracts and shows the value.

-   Make `Identity` an instance of the `Monad` class.

    `Identity` encapsulates the identity function by types, `return` being the identity function, and `>>=` is the application operator in postfixed form.

The basic idea to convert a program to its monadic form is as follows: a function of type `a -> b` is converted to one of type `a -> M b`.

Therefore, in the definition of the `Value` type, the functions have the value `Value -> M Value` instead of `Value -> Value`, and therefore the interpreter function will also have the type `Term -> Environment -> M Value`.

Just as the `Value` type represents a value, the `M Value` type can be thought of as a computation that produces a value and an effect.

**Requirement:** Based on the MicroHaskell interpreter, define a monadic interpreter for the above language.

```haskell
type Environment = [(Name, Value)]
interp :: Term -> Environment -> M Value
```

The identity function has the type `a -> a`. The corresponding function in monadic form is `return`, which has the type `a -> M a`. `return` transforms a value into its corresponding representation in the monad.

For example the definition of `interp` for constants is:

```haskell
interp (Con i) _ = return (Num i)
```

The expression `(Num i)` has the `Value` type, so applying `return` we obtain a value of type `M Value` corresponding to the resulting type of `interp`.

For more interesting cases we will use the notation `do`. For example, the case for the addition operator:

```haskell
interp (t1 :+: t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    add v1 v2
```

It can be read as follows:

-   evaluate t1, put the result in v1
-   evaluate t2, put the result in v2
-   add v1 to v2

To be able to test the interpreter, define `M` as `Identity`:

```haskell
type M = Identity
```

For this variant of the interpreter, evaluating (and displaying) `interp term0 []` we get `"42"` as expected.

## Exercise: partial evaluation (variant 1)

Instead of using `Wrong` to record failed ratings, define `M` as `Maybe` and use `Nothing` for failing ratings.

Remove `Wrong` and all his appearances from the interpreter's definition. Monada `Maybe` is predefined so you don't have to define it in the solution file.

## Exercise: error messages (variant 2)

To improve the error messages, use the (default) `Either` monad.
To change the interpreter, define `M` as `Either String` and replace each occurrence of return `Wrong` with an appropriate `Left` expression.

Possible error messages:

-   `unbound variable: <name>`
-   `should be numbers: <v1>, <v2>`
-   `should be function: <v1>`

The `interp term0 []` evaluation should be `Right 42`; the `interp (App (Con 7) (Con 2)) []` evaluation should be Left `"should be function: 7"`.

In an impure language, this change could have been made through exceptions.

## Exercise: non-deterministic choice (variant 3)

We will now modify the interpreter to model a non-deterministic language for which the evaluation returns the list of possible answers.

To do this, we will use the (pedefined) monad associated with the list type:

```haskell
type M a = [a]
```

Extend the language with two new expression constructors: `Fail` and `Both Term Term`.

`Fail`'s evaluation should return no value, while `Both u v`'s evaluation should return all values returned by `u` or `v`.

Extend the `interp` to get this semantics.

For example, the evaluation of `interp (App (Lam "x" (Var "x": +: Var "x")) (Both (Con 1) (Con 2)))) []` should be `[2,4]`.

This change is harder to think of in an impure language.

## Exercise: interpretation using `Reader` monad (variant 4)

We can think of the environment as a state that is read when we need the values of variables, but is not changed.

The monad of immutable states is the `Reader` monad.

Unlike state transformation, in this case the state does not change; so we no longer need its value after the execution of the computer. Therefore, the immutable state computation will be represented by a function which, given a state, produces a value corresponding to that state.

In order not to complicate things, we will instantiate the `Reader` monad for the `Environment` type of the evaluation environments:

```haskell
newtype EnvReader a = Reader { runEnvReader :: Environment -> a }
```

-   Make the `EnvReader` an instance of the `Show` class by displaying the value obtained by executing the computation in the initial evaluation environment `[]`.

-   Make `EnvReader` an instance of the `Monad` class.

    The `return` function returns the given value for any initial state

    The function `>>=` takes as arguments a computation `ma :: EnvReader a` and a function `k :: a -> EnvReader b`. Its result encapsulates a function from `Environment` to type `b` which

    -   sends the initial state to the state transformation `ma`; thus obtains a value.

    -   applies the function `k` to the value, obtaining a new computation

    -   this new computation receives the same initial state as the state and returns the result of the evaluation

Modify the interpreter to evaluate in the `EnvReader` monad. For this:

-   Remove the `Environment` argument from the `interp` and all other helper functions

-   Define a `ask :: EnvReader Environment` computation that returns the current state as the value

-   Use `ask` to define the semantics of the variables

-   Define a function `local :: (Environment -> Environment) -> EnvReader a -> EnvReader a` with the following semantics:

    -   `local f ma` takes a state transformation `f` and a computation `ma` and produces a new computation that will be executed in the modified current state using the function `f`

-   Use the `local` function to define `Lam`'s semantics, locally extending the evaluation environment to associate the given value with the variable

The evaluation of `interp term0` should return `"42"`.

## Exercise: displaying intermediate results (variant 5)

In this exercise we will change the interpreter to display.

We could use the `State` monad, but it is not the best choice, because the accumulation of results in a final state implies that the state will not be able to be displayed until the end of the computation.

Instead, we'll use the `Writer` monad. In order not to complicate the presentation, we will instantiate the type of the output channel to `String`.

```haskell
newtype StringWriter a = StringWriter { runStringWriter :: (a, String) }
```

-   Make `StringWriter` an instance of the `Show` class so that it displays the output string, followed by the resulting value.

-   Make `StringWriter` instant for the `Monad` class. `StringWriter` monad behaves like this:

    -   Each value is paired with the output string produced during the calculation of that value

    -   The `return` function returns the given value and produces nothing at the output

    -   The function `>>=` performs an application and concatenates the output of the first argument and the output produced by the application

-   Define a function `tell :: String -> StringWriter ()` that displays the given value as an argument.

-   Extend the language with a display operation by adding the term `Out Term`.

    The evaluation of `Out u` displays the value of `u`, followed by `;` and returns that value.

    For example, `interp (Out (Con 41): +: Out (Con 1)) []` should display `"Output: 41; 1; Value: 42"`.

In an impure language, this change could be made using the display as a side effect.

## Exercise: state (variant 6)

To illustrate state manipulation, we will modify the interpreter to calculate the number of steps required to calculate the result.

The same technique can be used to give semantics to other constructions that require state such as pointers and heap.

The monad of state transformations is the `State` monad.

A state transformation is a function that takes an initial state and returns a pair between a value and the new state. In order not to complicate things, we will instantiate the state to the `Integer` type needed in this exercise:

```haskell
newtype IntState a = IntState { runIntState :: Integer -> (a, Integer) }
```

-   Make `IntState a` the instance of the `Show` class by displaying the value and the final state obtained by executing the state transformation in the initial state 0.

-   Make `IntState` the instance of the `Monad` class

    The `return` function returns the given value and propagates the state unchanged.

    The function `>>=` takes a state transformation `ma :: IntState a` and a function `k :: a -> IntState b`. Its result encapsulates a state transformation which:

    -   sends the initial state to the state transformation `ma`; thus obtains an intermediate value and state

    -   applies the function `k` to the value, obtaining a new state transformation

    -   this new state transformation receives as initial state the intermediate state obtained following the evaluation of `ma`; it returns the result and the final state

The evaluation of `interp term0 []` should return `"Value: 42; Count: 3"`.

To achieve this:

-   define the function `modify :: (Integer -> Integer) -> IntState ()` which modifies the internal state of the monad according to the function given as an argument

-   define a computation that increases the counter: `tickS :: IntState ()` and modify the addition and application evaluations using `tickS` to increase the counter for each of their calls

We can extend the language to allow access to the current value of the execution counter.

-   Define the `get :: IntState Integer` computation that gets the current value of the counter as the value

-   Extend the `Term` type with a new `Count` constructor

-   Define the `interp` for the `Count` with the semantics to get the number of steps executed so far and return it as the `Num` value corresponding to the term

For example, `interp ((Con 1: +: Con 2): +: Count) []` should display `"Value: 4; Count: 2"`, because only one addition occurs before `Count`'s evaluation.

In an impure language, these changes could be made using a global variable / memory location to hold the counter.
