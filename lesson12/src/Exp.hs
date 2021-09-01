import Parsing
import Control.Applicative
import Data.Char
import Data.Maybe

-- --------------------------
-- ------ PARSING -----------
-- --------------------------

data Exp 
    = Lit Int
    | Id String
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp
    deriving (Eq,Show)

type Env = [(String, Int)]

myLookUp :: String -> Env -> Int
myLookUp s env = fromMaybe 0 (lookup s env)

evalExp   :: Exp -> Env -> Int
evalExp   (Lit n)   _   = n
evalExp   (Id s)    env = myLookUp s env 
evalExp   (Add e f) env = evalExp e env + evalExp f env 
evalExp   (Sub e f) env = evalExp e env - evalExp f env 
evalExp   (Mul e f) env = evalExp e env * evalExp f env 
evalExp   (Div e f) env = evalExp e env `div` evalExp f env 

parseId :: Parser String
parseId = pure (:) <*> satisfy isAlpha <*> many (satisfy idChar)
    where
        idChar c = isAlphaNum c || c == '_'

parseExp :: Parser Exp
parseExp =
        parseTerm
        <|>
        pure Add <*> parseTerm <* token (char '+') <*> parseExp
        <|>
        pure Sub <*> parseTerm <* token (char '-') <*> parseTerm
    where
        parseTerm =
            parseFactor
            <|>
            pure Mul <*> parseFactor <* token (char '*') <*> parseTerm
            <|>
            pure Div <*> parseFactor <* token (char '/') <*> parseFactor
        
        parseFactor =
            Lit <$> parseInt
            <|> 
            Id <$> parseId
            <|>
            char '(' *> skipSpace *> parseExp <* skipSpace <* char ')'

parseEvalExp :: String -> Env -> Int
parseEvalExp s = evalExp e
    where
        e = parse parseExp s
