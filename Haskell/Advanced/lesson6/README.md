# Lesson 6

## `SIMPLE` Type checker

## Abstract syntax

```haskell
type Name = String

data BinAop = Add | Mul | Sub | Div | Mod

data BinCop = Lt | Lte | Gt | Gte

data BinEop = Eq | Neq

data BinLop = And | Or

data Exp
    = Id Name
    | I Integer
    | B Bool
    | UMin Exp
    | BinA BinAop Exp Exp
    | BinC BinCop Exp Exp
    | BinE BinEop Exp Exp
    | BinL BinLop Exp Exp
    | Not Exp

data Stmt
    = Asgn Name Exp
    | If Exp Stmt Stmt
    | Read String Name
    | Print String Exp
    | While Exp Stmt
    | Block [Stmt]
    | Decl Name Exp
    deriving (Show)
```

We will use a "state" in which each variable has an associated type; "States" are defined using `Data.Map`.

```haskell
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type CheckerState = Map Name Type

emptyCheckerState :: CheckerState
emptyCheckerState = Map.empty
```

The check function will associate a syntactic construction with an `M Type` value, where `M` is a monad and `Type` is a type:

```haskell
data Type = TInt | TBool
    deriving (Eq)
```

The "unit" `()` type will be the type of instructions.

Monad `M` is a combination of `Monad Reader` and `Monad Either`.

```haskell
newtype EReader a =
    EReader { runEReader :: CheckerState -> (Either String a) }

instance Monad EReader where
    return a = EReader (\env -> Right a)
    act >>= k = EReader f
        where
            f env = case (runEReader act env) of
                        Left s -> Left s
                        Right va -> runEReader (k va) env

type M = EReader
```

## Exercise

Implement the following functions to define a type checker for the above language using the `EReader` monad.

```haskell
checkExp :: Exp -> M Type
checkStmt :: Stmt -> M ()
checkBlock :: [ Stmt ] -> M ()
checkPgm :: [ Stmt ] -> Bool
```
