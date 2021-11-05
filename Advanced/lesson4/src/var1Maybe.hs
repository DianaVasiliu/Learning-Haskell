type M = Maybe

showM :: Show a => M a -> String
showM (Just a) = show a
showM Nothing = "Nothing"

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
add (Num n1) (Num n2) = return (Num (n1 + n2))
add _ _ = Nothing

app :: Value -> Value -> M Value
app (Fun f) v = f v
app _ _ = Nothing

interp :: Term -> Environment -> M Value
interp (Var v) env = lookup v env

interp (Con x) _ = return (Num x)

interp (t1 :+: t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    add v1 v2

interp (Lam var term) env = return $ Fun (\v -> interp term ((var, v) : env))

interp (App t1 t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    app v1 v2

-- tests
test :: Term -> String
test t = showM $ interp t []

pgm1:: Term
pgm1 = App
            (Lam "x" (Var "x" :+: Var "x"))
            (Con 10 :+:  Con 11)


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

pgm2 :: Term
pgm2 = App
        (Lam "x" (Var "x" :+: Var "y"))
        (Con 10 :+: Con 11)

pgm3 :: Term
pgm3 = App
        (Var "x" :+: Var "y")
        (Con 10) :+: Con 11

test0 = test term0
test1 = test pgm
test2 = test pgm1
test3 = test pgm2
test4 = test pgm3
