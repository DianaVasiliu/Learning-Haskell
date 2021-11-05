newtype EnvReader a = Reader { runEnvReader :: Environment -> a }

type M = EnvReader

instance Show a => Show (EnvReader a) where
    show = showM

instance Monad EnvReader where
    return a = Reader (const a)
    ma >>= k = Reader f 
        where
            f env = let va = runEnvReader ma env
                    in  runEnvReader (k va) env

instance Applicative EnvReader where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)

instance Functor EnvReader where
    fmap f ma = pure f <*> ma

ask :: EnvReader Environment
ask = Reader id

local :: (Environment -> Environment) -> EnvReader a -> EnvReader a
local f ma = Reader (\e -> 
                        let e1 = f e
                        in  runEnvReader ma e1
                    )

showM :: Show a => M a -> String
showM ma = show $ runEnvReader ma []

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
    | Wrong

instance Show Value where
    show (Num x) = show x
    show (Fun _) = "<function>"
    show Wrong   = "<wrong>"

type Environment = [(Name, Value)]

add :: Value -> Value -> M Value
add (Num nr1) (Num nr2) = return (Num (nr1 + nr2))
add _ _                 = return Wrong

app :: Value -> Value -> M Value
app (Fun f) v = f v
app _ _ = return Wrong

interp :: Term -> M Value
interp (Var name) = do
    env <- ask 
    case lookup name env of
        Just v  -> return v
        Nothing -> return Wrong

interp (Con i) = return (Num i)

interp (t1 :+: t2) = do
    v1 <- interp t1
    v2 <- interp t2
    add v1 v2

interp (Lam name term) = do
    env <- ask
    return $ Fun (\v -> local (\_ -> (name, v) : env) (interp term))

interp (App term1 term2) = do
    t1 <- interp term1
    t2 <- interp term2
    app t1 t2

test :: Term -> String
test t = showM $ interp t

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

test0 = test term0
test1 = test pgm
test2 = test pgm1
test3 = test pgm2
test4 = test pgm3