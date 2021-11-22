import Data.Maybe

------------------------------
-- A Mini-Computer Language --
------------------------------

data Prog = On Stmt
data Stmt 
    = Off 
    | Expr :> Stmt
data Expr 
    = Mem 
    | V Int 
    | Expr :+ Expr
type Env = Int -- the memory value
type DomProg = [Int]
type DomInstr = Env -> [Int]
type DomExpr = Env -> Int

-- prog :: Prog -> [Int]
prog :: Prog -> DomProg
prog (On st) = stmt st 0

-- stmt :: Stmt -> Env -> [Int]
stmt :: Stmt -> DomInstr
stmt Off mem = []
stmt (exp :> st) mem = 
    let 
        e = expr exp mem 
        i = stmt st mem
    in 
        e : i

-- expr :: Expr -> Env -> Int
expr :: Expr -> DomExpr
expr Mem mem            = mem
expr (V var) mem        = var
expr (exp1 :+ exp2) mem = expr exp1 mem + expr exp2 mem

p1 = On (V 3 :> ((Mem :+ V 5):> Off))
test = prog p1


------------------
-- Mini-Haskell --
------------------

type Name = String
data Hask 
    = HTrue
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

data Value 
    =  VBool Bool
    | VInt Int
    | VFun (Value -> Value)
    | VError 
type HEnv = [(Name, Value)]

type DomHask = HEnv -> Value

-- 1)
instance Show Value where
    show (VBool b)  = show b
    show (VInt i)   = show i
    show (VFun f)   = "Function"
    show VError     = "Error"

-- 2)
instance Eq Value where
    (VBool a) == (VBool b)  = a == b
    (VInt a) == (VInt b)    = a == b
    (VFun _) == (VFun _)    = error "Cannot compare functions"
    VError == VError        = error "Cannot compare errors"
    _ == _                  = False

-- 3)
-- hEval :: Hask -> [(Name, Value)] -> Value
hEval :: Hask -> DomHask
hEval HTrue _       = VBool True
hEval HFalse  _     = VBool False
hEval (HLit x) _    = VInt x

hEval (HIf cond s1 s2) env = 
    evalIf (hEval cond env) (hEval s1 env) (hEval s2 env)
    where
        evalIf :: Value -> Value -> Value -> Value
        evalIf (VBool b) thenClause elseClause = 
            if b
                then thenClause
                else elseClause
        evalIf _ _ _ = VError

hEval (h1 :==: h2) env = 
    evalEq (hEval h1 env) (hEval h2 env)
    where
        evalEq :: Value -> Value -> Value
        evalEq (VBool b1) (VBool b2)    = VBool (b1 == b2)
        evalEq (VInt n1) (VInt n2)      = VBool (n1 == n2)
        evalEq _ _                      = VError

hEval (h1 :+: h2) env = 
    evalPlus (hEval h1 env) (hEval h2 env)
    where
        evalPlus :: Value -> Value -> Value
        evalPlus (VInt n1) (VInt n2)    = VInt (n1 + n2)
        evalPlus _ _                    = VError

hEval (HVar var) env = fromMaybe VError (lookup var env)

hEval (HLam var body) env = VFun (\v -> hEval body ((var, v) : env))

hEval (h1 :$: h2) env = 
    evalAppl (hEval h1 env) (hEval h2 env)
    where 
        evalAppl :: Value -> Value -> Value
        evalAppl (VFun f) v = f v
        evalAppl _ _        = VError

pr = HLam "x" (HLam "y" (HVar "x" :+: HVar "y")) :$: HLit 3 :$: HLit 4
test2 = hEval pr []
