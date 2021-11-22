import Data.List (nub)
import Data.Maybe (fromJust)

type Name = String

data Prop
    = Var Name
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    | Prop :->: Prop
    | Prop :<->: Prop
    deriving Eq

infixr 2 :|:
infixr 3 :&:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R"))
    :&: 
    ((Not (Var "P") :|: Not (Var "Q"))
    :&: 
    (Not (Var "P") :|: Not (Var "R")))

instance Show Prop where
    show (Var v)    = v
    show T          = "T"
    show F          = "F"
    show (Not p)    = "(~" ++ show p ++ ")"
    show (p :|: q)  = "(" ++ show p ++ "|" ++ show q ++ ")"
    show (p :&: q)  = "(" ++ show p ++ "&" ++ show q ++ ")"
    show (p :->: q)  = "(" ++ show p ++ "->" ++ show q ++ ")"
    show (p :<->: q)  = "(" ++ show p ++ "<->" ++ show q ++ ")"

test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

type Env = [(Name, Bool)]

impureLookup :: Eq a => a -> [(a, b)] -> b
impureLookup a = fromJust . lookup a

impl :: Bool -> Bool -> Bool
impl False _ = True
impl _ x = x

eval :: Prop -> Env -> Bool
eval T _                = True
eval F _                = False
eval (Var v) env        = impureLookup v env
eval (Not p) env        = not $ eval p env
eval (p :|: q) env      = eval p env || eval q env
eval (p :&: q) env      = eval p env && eval q env
eval (p :->: q) env     = eval p env `impl` eval q env
eval (p :<->: q) env    = eval p env == eval q env

test_eval :: Bool
test_eval = eval (Var "P" :|: Var "Q") [("P", True), ("Q", False)]

variables :: Prop -> [Name]
variables (Var v)       = [v]
variables (Not p)       = nub $ variables p
variables (p :|: q)     = nub $ variables p ++ variables q
variables (p :&: q)     = nub $ variables p ++ variables q
variables (p :->: q)    = nub $ variables p ++ variables q
variables (p :<->: q)     = nub $ variables p ++ variables q
variables _ = []

test_variables :: Bool
test_variables =
    variables (Not (Var "P") :&: Var "Q") == ["P", "Q"]

envs :: [Name] -> [Env]
envs []     = []
envs [x]    = [[(x, True)], [(x, False)]]
envs (x:xs) = [(x, bv) : e | bv <- booleans, e <- envs xs]
    where
        booleans = [False, True]

test_envs :: Bool
test_envs =
    envs ["P", "Q"]
    ==
    [[("P",False)
    , ("Q",True)
    ]
    ,[("P",False)
    , ("Q",False)
    ]
    ,[("P",True)
    , ("Q",True)
    ]
    ,[("P",True)
    , ("Q",False)
    ]
    ]

satisfiable :: Prop -> Bool
satisfiable p = or evals
    where
        allVars = variables p
        allPairs = envs allVars
        evals = map (eval p) allPairs

test_satisfiable1 :: Bool
test_satisfiable1 = satisfiable (Not (Var "P") :&: Var "Q") == True

test_satisfiable2 :: Bool
test_satisfiable2 = satisfiable (Not (Var "P") :&: Var "P") == False

valid :: Prop -> Bool
valid p = not $ satisfiable (Not p)

test_valid1 :: Bool
test_valid1 = valid (Not (Var "P") :&: Var "Q") == False

test_valid2 :: Bool
test_valid2 = valid (Not (Var "P") :|: Var "P") == True

truthTable :: Prop -> String
truthTable p = concat varList
            ++ "| "
            ++ show p
            ++ "\n"
            ++ concat (replicate numVars "- ")
            ++ "| "
            ++ concat (replicate formulaLength "-")
            ++ "\n"
            ++ showTable p env 
    where
        vars = variables p
        varList = [v ++ " " | v <- vars]
        numVars = length varList
        formulaLength = length (show p)
        env = envs vars
        
        showTable :: Prop -> [Env] -> String
        showTable _ [] = ""
        showTable p (e : env) = getSnd e
                            ++ "| "
                            ++ replicate (formulaLength `div` 2) ' '
                            ++ showBool (eval p e)
                            ++ "\n"
                            ++ showTable p env
        
        getSnd :: Env -> String
        getSnd [] = ""
        getSnd ((_, val) : env) = showBool val ++ getSnd env

        showBool :: Bool -> String
        showBool True = "T "
        showBool False = "F "

equivalent :: Prop -> Prop -> Bool
equivalent p q = and [eval (p :<->: q) env | env <- environments]
    where
        vars = nub $ variables p ++ variables q
        environments = envs vars

test_equivalent1 :: Bool
test_equivalent1 =
    True
    ==
    (Var "P" :&: Var "Q") `equivalent` (Not (Not (Var "P") :|: Not (Var "Q")))

test_equivalent2 :: Bool
test_equivalent2 =
    False
    ==
    (Var "P") `equivalent` (Var "Q")

test_equivalent3 :: Bool
test_equivalent3 =
    True
    ==
    (Var "R" :|: Not (Var "R")) `equivalent` (Var "Q" :|: Not (Var "Q"))