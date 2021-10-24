# Lesson 1

## A mini-computer language

We define the following language of a mini-computer:

```haskell
data Prog = On Stmt
data Stmt = Off | Expr :> Stmt
data Expr = Mem | V Int | Expr :+ Expr
type Env = Int -- the memory value
type DomProg = [Int]
type DomInstr = Env -> [Int]
type DomExpr = Env -> Int
```

We want to write a program that prints the list of the expressions' computed values, where `Mem` is the last computed value. The initial value of `Mem` is 0.

Example:

`On ((V 3) :> ((Mem :+ (V 5)) :> Off))`

will print `[3, 8]`.

### Exercise

Define the evaluation functions for the `Prog`, Stmt`and`Expr` expressions.

```haskell
prog :: Prog -> DomProg
stmt :: Stmt -> DomInstr
expr :: Expr -> DomExpr
```

## Mini-Haskell

We will define, using Haskell, a mini functional language and its semantic.

The Mini-Haskell language contains:

-   `Bool` and `Int` expressions
-   lambda expressions (functions)
-   function application expressions

```haskell
type Name = String
data Hask =
   HTrue
 | HFalse
 | HLit Int
 | HIf Hask Hask Hask
 | Hask :==: Hask
 | Hask :+: Hask
 | HVar Name
 | HLam Name Hask
 | Hask :$: Hask
  deriving (Read, Show)
infix 4 :==:
infixl 6 :+:
infixl 9 :$:
```

To evaluate (interpret) the expressions, we will define an evaluation environment (the memory) in which we will memoize the variables and the current associated values.

```haskell
data Value
 = VBool Bool
 | VInt Int
 | VFun (Value -> Value)
 | VError
type HEnv = [(Name, Value)]

type DomHask = HEnv -> Value
```

### Exercise

1. Make the `Value` data type an instance of the `Show` class.

2. Make the `Value` data type an instance of the `Eq` class. In case the values cannot be compared (functions or errors), an error will be thrown using the `error` function.

3. Write the evaluation function for a `Hask` expression.

```haskell
hEval :: Hask -> DomHask
```
