import Data.Maybe
import Data.List

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

factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3 :::
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1)
    )
    
pg1 = Pgm [] factStmt


aEval :: AExp -> Env -> Integer
aEval (Lit i) _ = i
aEval (ae1 :+: ae2) env = aEval ae1 env + aEval ae2 env
aEval (ae1 :*: ae2) env = aEval ae1 env * aEval ae2 env
aEval (Var name) env = fromMaybe (error "Variable not found") (lookup name env)

bEval :: BExp -> Env -> Bool
bEval BTrue _ = True
bEval BFalse _ = False
bEval (ae1 :==: ae2) env = aEval ae1 env == aEval ae2 env
bEval (Not b) env = not (bEval b env)

sEval :: Stmt -> Env -> Env
sEval Skip env = env
sEval (s1 ::: s2) env = 
    let 
        e2 = sEval s1 env
    in 
        sEval s2 e2
sEval (If b s1 s2) env = 
    if bEval b env
        then sEval s1 env
        else sEval s2 env
sEval (While b s) env =
    if bEval b env
        then sEval (While b s) (sEval s env)
        else env
sEval (var := aexp) env =
    let 
        filteredEnv = filter ((/= var) . fst) env
    in
        (var, aEval aexp env) : filteredEnv
    
pEval :: Pgm -> Env
pEval (Pgm vars stats) = 
    sEval stats [(v, 0) | v <- vars]
