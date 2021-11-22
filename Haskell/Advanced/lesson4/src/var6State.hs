newtype IntState a = IntState { runIntState :: Integer -> (a, Integer) }

type M = IntState

instance Show a => Show (IntState a) where
    show = showM

instance Monad IntState where
    return a = IntState (\nr -> (a, nr))
    ma >>= k = IntState (\nr ->
                            let (va, rez1) = runIntState ma nr
                                IntState vb = k va
                            in  vb rez1
                            )

instance Applicative IntState where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)

instance Functor IntState where
    fmap f ma = pure f <*> ma

showM :: Show a => M a -> String
showM ma = "Value: " ++ show a ++ "; Count: " ++ show c
    where
        (a, c) = runIntState ma 0

modify :: (Integer -> Integer) -> IntState ()
modify f = IntState (\s -> ((), f s))

tickS :: IntState ()
tickS = modify (+1)

get :: IntState Integer
get = IntState (\s -> (s, s))

put :: Integer -> IntState ()
put s = IntState (\_ -> ((), s))

type Name = String

data Term 
    = Var Name
    | Con Integer
    | Term :+: Term
    | Lam Name Term
    | App Term Term
    | Count
    deriving (Show)

data Value 
    = Num Integer
    | Fun (Value -> M Value)
    | Wrong

instance Show Value where
    show (Num x) = show x
    show (Fun _) = "<function>"
    show Wrong   = "<wrong>"

type Environment = [(Name, Value)]

add :: Value -> Value -> M Value
add (Num nr1) (Num nr2) = tickS >> return (Num (nr1 + nr2))
add _ _                 = return Wrong

app :: Value -> Value -> M Value
app (Fun f) v   = tickS >> f v
app _ _         = return Wrong

interp :: Term -> Environment -> M Value
interp (Var name) env =
    case lookup name env of
        Just v  -> return v
        Nothing -> return Wrong

interp (Con i) _ = return (Num i)

interp (t1 :+: t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    add v1 v2

interp (Lam name term) env = return $ Fun (\v -> interp term ((name, v) : env))

interp (App term1 term2) env = do
    t1 <- interp term1 env 
    t2 <- interp term2 env
    app t1 t2

interp Count env = do
    i <- get
    return (Num i)

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
            (Con 10 :+: Con 11)

pgm2 :: Term
pgm2 = App
        (Lam "x" (Var "x" :+: Var "y"))
        (Con 10 :+: Con 11)

pgm3 :: Term
pgm3 = App
        (Var "x" :+: Var "y")
        (Con 10) :+: Con 11

pgm4 :: Term
pgm4 = (Con 1 :+: Con 2) :+: Count

test0 = test term0
test1 = test pgm
test2 = test pgm1
test3 = test pgm2
test4 = test pgm3
test5 = test pgm4