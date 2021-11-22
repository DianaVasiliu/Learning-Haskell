module Module where

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Parse (loadProgramFromFile)
import AST
import Checker (checkPgm)

import System.IO (stdout, hFlush)
import System.Environment ( getArgs )   


data Value = IVal Integer  | BVal Bool
    deriving (Show, Eq)

data ImpState = ImpState
    { env :: Map String Int
    , store :: Map Int Value
    , nextLoc :: Int
    }
    deriving (Show)

compose :: Ord b => Map b c -> Map a b -> Map a c
compose bc ab
    | null bc = Map.empty
    | otherwise = Map.mapMaybe (bc Map.!?) ab

showImpState :: ImpState -> String
showImpState st = 
    "Final state: " <> show (compose (store st) (env st))

emptyState :: ImpState
emptyState = ImpState Map.empty Map.empty 0

type M = StateT ImpState IO

runM :: M a -> IO (a, ImpState)
runM m = runStateT m emptyState

lookupM :: String -> M Value
lookupM x = do
    Just l <- Map.lookup x <$> gets env
    Just v <- Map.lookup l <$> gets store
    return v

updateM :: String -> Value -> M ()
updateM x v = do
    Just l <- Map.lookup x <$> gets env
    st <- gets store
    let st' = Map.insert l v st
    modify' (\s -> s {store = st'})

-- Operators interpretor

cop :: BinCop  -> Integer -> Integer -> Bool
cop Lt = (<)
cop Lte = (<=)
cop Gt = (>)
cop Gte = (>=)

aop :: BinAop -> Integer -> Integer -> Integer
aop Add = (+)
aop Mul = (*)
aop Sub = (-)
aop Div = div
aop Mod = mod

eop :: BinEop -> Value -> Value -> Bool
eop Eq = (==)
eop Neq = (/=)

lop :: BinLop -> Bool -> Bool -> Bool
lop And = (&&)
lop Or = (||)

-- Expressions interpretor

evalExp :: Exp -> M Value
evalExp (Id x) = lookupM x
evalExp (I x) = return (IVal x)
evalExp (B x) = return (BVal x)
evalExp (UMin e) = do
    IVal i <- evalExp e
    return (IVal $ -i)
evalExp (BinA op e1 e2) = do
    IVal i1 <- evalExp e1
    IVal i2 <- evalExp e2
    return (IVal $ aop op i1 i2)
evalExp (BinC op e1 e2) = do
    IVal i1 <- evalExp e1
    IVal i2 <- evalExp e2
    return (BVal $ cop op i1 i2)
evalExp (BinE op e1 e2) = do
    v1 <- evalExp e1
    v2 <- evalExp e2
    return (BVal $ eop op v1 v2)
evalExp (BinL op e1 e2) = do
    BVal b1 <- evalExp e1
    BVal b2 <- evalExp e2
    return (BVal $ lop op b1 b2)
evalExp (Not e) = do
    BVal b <- evalExp e
    return (BVal $ not b)

-- evalExp e = error $ "Evaluation for '" <> show e <> "' not yet defined."

-- Statements interpretor

evalStmt :: Stmt -> M ()
evalStmt (Asgn x e) = do
    v <- evalExp e
    updateM x v

evalStmt (If exp s1 s2) = do
    BVal b <- evalExp exp
    if b
        then evalStmt s1
        else evalStmt s2

evalStmt (Read s x) = do
    i <- liftIO (putStr s >> hFlush stdout >> readLn)
    evalStmt(Asgn x (I i))

evalStmt (Print text e) = do
    IVal i <- evalExp e
    liftIO $ putStrLn (text ++ " " ++ show i ++ ";")

evalStmt (While e s) = do
    BVal b <- evalExp e
    when b
        (evalStmt s >> evalStmt (While e s))

evalStmt (Block sts) = do
    oldEnv <- gets env
    mapM_ evalStmt sts
    modify' (\s -> s {env = oldEnv})

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

evalStmt (PPlus var) = do
    IVal v <- evalExp (Id var)
    evalStmt (Asgn var (I (v + 1)))

-- evalStmt s = error $ "Evaluation for '" <> show s <> "' not yet defined."

-- Program interpretor

evalPgm :: [Stmt] -> IO ((), ImpState)
evalPgm sts = runM $ mapM_ evalStmt sts

main :: IO ()
main = do
    args <- getArgs
    when (null args) (error "Need file to run")
    let (file:_) = args
    pgm <- loadProgramFromFile file
    checkPgm pgm
    (_, st) <- evalPgm pgm
    putStrLn $ showImpState st

-- :main "1.imp"
