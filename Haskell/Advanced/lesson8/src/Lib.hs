import Data.List

data Variable = Variable String Int

instance Eq Variable where
    Variable x1 i1 == Variable x2 i2 = x1 == x2 && i1 == i2

var :: String -> Variable
var x = Variable x 0

-- 1)
fresh :: Variable -> [Variable] -> Variable
fresh (Variable x _) [] = var x
fresh (Variable x _) xs = Variable x (getMaxIdx (filtered xs) + 1)
    where 
        filtered :: [Variable] -> [Variable]
        filtered lst = filter (\(Variable n i) -> n == x) lst
        getMaxIdx :: [Variable] -> Int
        getMaxIdx [] = 0
        getMaxIdx ((Variable n i) : lst) = max i (getMaxIdx lst)

testFresh1 :: Bool
testFresh1 = fresh (Variable "x" 0) [Variable "y" 2, Variable "x" 1] == Variable "x" 2

testFresh2 :: Bool
testFresh2 = fresh (Variable "x" 0) [] == Variable "x" 0

-- 2)
instance Show Variable where
    show (Variable x 0) = x
    show (Variable x i) = x ++ "_" ++ show i

data Term 
    = V Variable
    | App Term Term
    | Lam Variable Term
    deriving Eq

v :: String -> Term
v x = V (var x)

lam :: String -> Term -> Term
lam x = Lam (var x)

lams :: [String] -> Term -> Term
lams xs t = foldr lam t xs

($$) :: Term -> Term -> Term
($$) = App

infixl 9 $$

-- 3)
instance Show Term where
    show (V x) = show x
    show (Lam v t) = "(\\" ++ show v ++ "." ++ show t ++ ")"
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

testShowTerm1 :: Bool
testShowTerm1 = show (v "x") == "x"

testShowTerm2 :: Bool
testShowTerm2 = show (v "x" $$ v "y") == "(x y)"

testShowTerm3 :: Bool
testShowTerm3 = show (lam "x" (v "x")) == "(\\x.x)"

testShowTerm4 :: Bool
testShowTerm4 = show (lams ["x", "y"] (v "x")) == "(\\x.(\\y.x))"

-- 4)
freeVars :: Term -> [Variable]
freeVars (V v) = [v]
freeVars (App t1 t2) = nub $ freeVars t1 ++ freeVars t2
freeVars (Lam v t) = nub [vars | vars <- freeVars t, vars /= v]

testFreeVars :: Bool
testFreeVars = freeVars (lam "x" (v "x" $$ v "y")) == [var "y"]

-- 5)
allVars :: Term -> [Variable]
allVars (V v) = [v]
allVars (App t1 t2) = nub $ allVars t1 ++ allVars t2
allVars (Lam v t) = nub $ v : allVars t

testAllVars :: Bool
testAllVars = allVars (lam "x" (v "x" $$ v "y")) == [var "x", var "y"]

-- 6)
subst :: Term -> Variable -> Term -> Term
subst u x (V y) 
    | x == y    = u
    | otherwise = V y

subst u x (App t1 t2) = App (subst u x t1) (subst u x t2)

subst u x (Lam y t) 
    | x == y                = Lam y t
    | y `notElem` freeVarsU = Lam y (subst u x t)
    | x `notElem` freeVarsT = Lam y t
    | otherwise             = Lam y' t'
    where
        freeVarsT = freeVars t
        freeVarsU = freeVars u
        allFreeVars = nub ([x] ++ freeVarsU ++ freeVarsT)
        y' = fresh y allFreeVars
        t' = subst (V y') y t

-- 7)
aEq :: Term -> Term -> Bool
aEq (V v1) (V v2) = v1 == v2
aEq (App t1 t2) (App t1' t2') = aEq t1 t1' && aEq t2 t2'
aEq (Lam x t) (Lam y u)
    | x == y = aEq t u
    | otherwise = aEq term1 term2
    where
        freeVarsT   = freeVars t
        freeVarsU   = freeVars u
        allFreeVars = nub $ [x, y] ++ freeVarsT ++ freeVarsU
        z           = V (fresh y allFreeVars)
        term1       = subst z x t
        term2       = subst z y u
aEq _ _ = False

testAEQ1 :: Bool
testAEQ1 = aEq (lam "x" (v "x" )) (lam "y" (v "y"))

testAEQ2 :: Bool
testAEQ2 = not $ aEq (lam "x" (v "x")) (lam "y" (v "z"))

-- 8)
reduceOnce :: Term -> Maybe Term
reduceOnce (App (Lam x t) u) = Just $ subst u x t
reduceOnce (App term1 term2) =
    case term1Reduced of 
        Just term1' -> Just $ App term1' term2
        Nothing     -> case term2Reduced of
                        Just term2' -> Just $ App term1 term2'
                        Nothing     -> Nothing
    where
        term1Reduced = reduceOnce term1
        term2Reduced = reduceOnce term2
reduceOnce (Lam x t) =
    case tReduced of
        Just t' -> Just $ Lam x t'
        Nothing -> Nothing
    where 
        tReduced = reduceOnce t
reduceOnce _ = Nothing

reduce :: Term -> Term
reduce term = case tReduced of
                Just term'  -> reduce term'
                Nothing     -> term
    where
        tReduced = reduceOnce term

-- testReduce :: Bool
-- testReduce = reduce
--     (lams ["m", "n"] (v "n" $$ v "m" ) 
--     $$ lams ["s", "z"] (v "s" $$ (v "s" $$ v "z"))
--     $$ lams ["s", "z"] (v "s" $$ (v "s" $$ (v "s" $$ v "z")))
--     )
--     ==
--     reduce (lams ["s", "z"] (v "s" $$ (v "s" $$ (v "s" $$ (v "s" $$ (v "s" $$ (v "s" $$ (v "s" $$ (v "s" $$ v "z")))))))))


-- 9)
abEq :: Term -> Term -> Bool
abEq t1 t2 = aEq (reduce t1) (reduce t2)

-- testTerm1 = lams ["m", "n"] (v "n" $$ v "m" )
--     $$ lams ["s", "z"] (v "s" $$ (v "s" $$ v "z"))
--     $$ lams ["s", "z"] (v "s" $$ (v "s" $$ (v "s" $$ v "z")))
    
-- testTerm2 = lams ["s", "z"] (v "s" $$ (v "s" $$ (v "s" $$ (v "s" $$ (v "s" $$ (v "s" $$ (v "s" $$ (v "s" $$ v "z"))))))))

-- testABEQ = abEq testTerm1 testTerm2
