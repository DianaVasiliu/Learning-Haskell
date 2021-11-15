# Lesson 7

Knowing the SIMPLE language syntax, implement an interpreter for this language, integrated with the parser and the type checker implemented in the previous lessons.

## Abstract syntax

### Expressions

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
```

### Statements

```haskell
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

## The interpreter

### Values

We have boolean and integer values.

```haskell
data Value
    = IVal Integer
    | BVal Bool
    deriving (Show, Eq)
```

### State (Environment and Store)

Because we have blocks and local variables, it is possible for a variable name to indicate different memory locations depending on the context (block) in which it appears. Therefore, we will express the state of the program using two maps: `store`, which represents the actual memory, associating values with memory locations; and `env`, which associate the variables visible in the current context with their locations in memory. In addition, to easily know what the next available location is when we allocate memory, we will store the last allocated location in the `nextLoc`.

```haskell
data ImpState = ImpState
    { env :: Map String Int
    , store :: Map Int Value
    , nextLoc :: Int
    }
    deriving (Show)

emptyState :: ImpState
emptyState = ImpState Map.empty Map.empty 0
```

### The interpreter monad

Because our interpreter will have as side effects both maintaining this state and the I/O interaction with the console, we will use a monad that combines these effects.

```haskell
type M = StateT ImpState IO

runM :: M a -> IO (a, ImpState)
runM m = runStateT m emptyState
```

In this monad, the operations corresponding to the `State` monad can be performed in the same way as in the `State` monad, and those of type I/O can be performed using the `liftIO :: IO a -> M a` command.

### Reading the current value of an identifier

1. we obtain from `env` the current location `l` associated with `x`
2. we obtain from `store` the value stored for location `l`

```haskell
lookupM :: String -> M Value
lookupM x = do
    Just l <- Map.lookup x <$> gets env
    Just v <- Map.lookup l <$> gets store
    return v
```

### Writing the current value of an identifier

1. we obtain from `env` the current location `l` associated with `x`
2. we update in `store` the value stored for location `l` to the value `v`
3. we update the status to make it visible in the future

```haskell
updateM :: String -> Value -> M ()
updateM x v = do
    Just l <- Map.lookup x <$> gets env
    st <- gets store
    let st' = Map.insert l v st
    modify' (\s -> s {store = st'})
```

## Exercise 1

In the `Lib.hs` file you will find a partial implementation of the `SIMPLE` language interpreter. Complete this implementation so that you can run programs with the `.imp` extension.

## `evalExp`

```haskell
evalExp :: Exp -> M Value
```

### Identifiers

To obtain the value of an identifier we use `lookupM`.

```haskell
evalExp (Id x) = lookupM x
```

To evaluate binary operators - we evaluate the expressions, waiting for results of the right types - fact guaranteed by the type checker - we apply the right operation to those values.

```haskell
evalExp (BinC op e1 e2) = do
    IVal i1 <- evalExp e1
    IVal i2 <- evalExp e2
    return (BVal $ cop op i1 i2)

cop :: BinCop -> Integer -> Integer -> Bool
cop Lt = (<)
cop _ = undefined
```

## `evalStmt`

```haskell
evalStmt :: Stmt -> M ()
```

### Assignment

The evaluation of the assignment is done by

-   evaluation of the expression that is assigned using `evalExp`
-   updating the value for the variable to which it is assigned using
    `updateM`

```haskell
evalStmt (Asgn x e) = do
    v <- evalExp e
    updateM x v
```

## Keyboard input

We use `liftIO` to execute the I/O action sequence

-   `putStr` - to display the read prompt
-   `hFlush stdout` - to ensure that the message was written
-   `readLn` - to read the value `i`

We perform an assignment operation to assign the newly read value to the variable in which it should be read.

```haskell
evalStmt (Read s x) = do
    i <- liftIO (putStr s >> hFlush stdout >> readLn)
    evalStmt(Asgn x (I i))
```

## Variable declaration

-   we evaluate the initialization expression to a value `v`
-   we store the value `v` at the `nextLoc` location in `store`
-   we set the `x` name to the `nextLoc` location in `env`
-   we grow with a unit of the `nextLoc` location

Notice the use of `modify'` to modify the content of the state.

```haskell
evalStmt (Decl x e) = do
    v <- evalExp e
    modify' (declare v)
    where
        declare v st = ImpState env' store' nextLoc'
        where
            l = nextLoc st
            nextLoc' = 1 + nextLoc st
            store' = Map.insert l v (store st)
            env' = Map.insert x l (env st)
```

## Blocks

The evaluation of a block is done as follows:

-   save the existing contents of `env` in `oldEnv`
-   the instructions in the block are executed (in order)
-   change the state to restore `env` to `oldEnv`

```haskell
evalStmt (Block sts) = do
    oldEnv <- gets env
    mapM_ evalStmt sts
    modify' (\s -> s {env = oldEnv})
```

## Exercise 2

Add a `++i` expression to the language (parser, type-checker, interpreter).
