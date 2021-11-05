type M = Either String

showM :: Show a => M a -> String
showM (Left s) = "Error: " ++ s
showM (Right a) = show a

type Name = String

data Term 
    = Var Name
    | Con Integer
    | Term :+: Term
    | Lam Name Term
    | App Term Term
    deriving (Show)

data Value 
    = Num Integer
    | Fun (Value -> M Value)

instance Show Value where
    show (Num x) = show x
    show (Fun _) = "<function>"

type Environment = [(Name, Value)]

add :: Value -> Value -> M Value
add (Num n1) (Num n2)   = return (Num (n1 + n2))
add n1 n2               = Left ("should be numbers: " ++ show n1 ++ ", " ++ show n2)

apply :: Value -> Value -> M Value
apply (Fun f) v = f v
apply f _ = Left ("should be function: " ++ show f)

interp :: Term -> Environment -> M Value
interp (Var name) env =  case lookup name env of
                            Just v  -> return v
                            Nothing -> Left ("unbound variable: " ++ name)

interp (Con c) _ = return (Num c)

interp (t1 :+: t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    add v1 v2

interp (Lam name term) env = return $ Fun (\v -> interp term ((name, v) : env))

interp (App t1 t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    apply v1 v2

-- tests
test :: Term -> String
test t = showM $ interp t []

term0 :: Term
term0 = App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11)

pgm :: Term
pgm = App
    (Lam "y"
        (App
        (App
            (Lam "f"
            (Lam "y"
                (App (Var "f") (Var "y"))
            )
            )
            (Lam "x"
            (Var "x" :+: Var "y")
            )
        )
        (Con 3)
        )
    )
    (Con 4) 

pgm1:: Term
pgm1 = App
            (Lam "x" (Var "x" :+: Var "x"))
            (Con 10 :+:  Con 11)

pgm2 :: Term
pgm2 = App
        (Lam "x" (Var "x" :+: Var "y"))
        (Con 10 :+: Con 11)

pgm3 :: Term
pgm3 = App
        (Var "x" :+: Var "y")
        (Con 10) :+: Con 11

pgm4 :: Term
pgm4 = App (Var "x") (Con 10 :+: Con 11)

test0 = test term0
test1 = test pgm
test2 = test pgm1
test3 = test pgm2
test4 = test pgm3
test5 = test pgm4