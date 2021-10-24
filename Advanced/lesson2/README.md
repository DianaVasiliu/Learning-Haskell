# Lesson 2

## A simple imperative language

We define the `IMP` language in Haskell:

```haskell
type Name = String

data Pgm = Pgm [Name] Stmt
        deriving (Read, Show)

data Stmt = Skip
          | Stmt ::: Stmt
          | If BExp Stmt Stmt
          | While BExp Stmt
          | Name := AExp
        deriving (Read, Show)

data AExp = Lit Integer
          | AExp :+: AExp
          | AExp :*: AExp
          | Var Name
        deriving (Read, Show)

data BExp = BTrue
          | BFalse
          | AExp :==: AExp
          | Not BExp
        deriving (Read, Show)

infixr 2 :::
infix 3 :=
infix 4 :==:
infixl 6 :+:
infixl 7 :*:

type Env = [(Name, Integer)]
```

We want from a program to print the state of the memory at the end of the execution. All the variables must be initialized before the program uses them. The variables are initialized by 0.

Example:

```haskell
factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3 :::
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1)
    )

pg1 = Pgm [] factStmt
```

will print `[("n",0),("p",6)]`.

## Exercise

Define the evaluation functions for `Pgm`, `Stmt`, `AExp` and `BExp` expressions.

```haskell
pEval :: Pgm -> Env
sEval :: Stmt -> Env -> Env
bEval :: BExp -> Env -> Bool
aEval :: AExp -> Env -> Integer
```
