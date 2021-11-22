module Lib where

import qualified Text.Parsec.Token as Token
import Text.Parsec.String ( Parser, parseFromFile )
import Text.Parsec.Expr 
        ( buildExpressionParser,
        Assoc(..),
        Operator(..) )
import Text.ParserCombinators.Parsec.Language 
        ( emptyDef,
        GenLanguageDef(..),
        LanguageDef )
import Text.Parsec 
        ( alphaNum, 
        letter, 
        (<|>), 
        eof,
        parseTest ) 

impLanguageDef :: LanguageDef ()

impLanguageDef = emptyDef
    {   commentStart = "/*", 
        commentEnd = "*/", 
        commentLine = "//", 
        nestedComments = False, 
        caseSensitive = True, 
        identStart = letter, 
        identLetter = alphaNum, 
        reservedNames = [ "while", "if", "else",
            "int", "bool", "true", "false", "read", "print"],
        reservedOpNames = ["+", "-", "*", "/", "%",
            "==", "!=", "<", "<=", ">=", ">", 
            "&&", "||", "!", "="]
    }


impLexer :: Token.TokenParser ()
impLexer = Token.makeTokenParser impLanguageDef

identifier :: Parser String
identifier = Token.identifier impLexer

reserved :: String -> Parser ()
reserved = Token.reserved impLexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp impLexer

parens :: Parser a -> Parser a
parens = Token.parens impLexer

braces :: Parser a -> Parser a
braces = Token.braces impLexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep impLexer

integer :: Parser Integer
integer = Token.integer impLexer

boolean :: Parser Bool
boolean = (reserved "true" >> return True) <|> (reserved "false" >> return False) 

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace impLexer

comma :: Parser String
comma = Token.comma impLexer

string :: Parser String
string = Token.stringLiteral impLexer


type Name = String

data BinAop = Add | Mul | Sub | Div | Mod
    deriving (Show)

data BinCop = Lt | Lte | Gt | Gte
    deriving (Show)

data BinEop = Eq | Neq
    deriving (Show)

data BinLop = And | Or
    deriving (Show)

data Exp
    = Id Name
    | I Integer
    | B Bool
    | UMin Exp
    | BinA BinAop Exp Exp
    | BinC BinCop Exp Exp
    | BinE BinEop Exp Exp
    | BinL BinLop Exp Exp
    | Not Exp
    deriving (Show)

data Type = TInt | TBool
    deriving (Eq, Show)

data Stmt
    = Asgn Name Exp     --
    | If Exp Stmt Stmt  --
    | Read String Name  --
    | Print String Exp  --
    | While Exp Stmt    -- 
    | Block [Stmt]      -- 
    | Decl Type Name    
    deriving (Show)

ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    cond <- parens expression
    thenS <- statement
    reserved "else"
    elseS <- statement
    return (If cond thenS elseS)

assignStmt :: Parser Stmt
assignStmt = do
    variable <- identifier
    reservedOp "="
    expr <- expression
    return (Asgn variable expr)

whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    cond <- parens expression
    body <- statement
    return (While cond body)

readStmt :: Parser Stmt
readStmt = do
    reserved "read"
    parens parseRead

parseRead :: Parser Stmt
parseRead = do
    str <- string
    comma
    var <- identifier
    return (Read str var)

printStmt :: Parser Stmt
printStmt = do
    reserved "print"
    parens parsePrint

parsePrint :: Parser Stmt
parsePrint = do
    str <- string
    comma
    var <- term
    return (Print str var)

blockStmt :: Parser Stmt
blockStmt = do
    braces parseBlock

parseBlock :: Parser Stmt
parseBlock = do
    list <- semiSep statement
    return (Block list)

declStmtInt :: Parser Stmt
declStmtInt = do
    reserved "int"
    var <- identifier
    return (Decl TInt var)

declStmtBool :: Parser Stmt
declStmtBool = do
    reserved "bool"
    var <- identifier
    return (Decl TBool var)


statement :: Parser Stmt
statement = 
    ifStmt <|> 
    whileStmt <|> 
    assignStmt <|> 
    readStmt <|> 
    printStmt <|> 
    blockStmt <|> 
    declStmtInt <|> 
    declStmtBool

expression :: Parser Exp
expression = buildExpressionParser operators term
    where
        operators =
            [ [ prefix "!" Not
                ],
                [ prefix "-" UMin
                ],
                [ binary "*" (BinA Mul) AssocLeft
                , binary "/" (BinA Div) AssocLeft
                , binary "%" (BinA Mod) AssocLeft
                ],
                [ binary "+" (BinA Add) AssocLeft
                , binary "-" (BinA Sub) AssocLeft
                ],
                [ binary "==" (BinE Eq) AssocNone
                , binary "!=" (BinE Neq) AssocNone
                , binary "<=" (BinC Lte) AssocNone
                , binary "<"  (BinC Lt) AssocNone
                , binary ">=" (BinC Gte) AssocNone
                , binary ">"  (BinC Gt) AssocNone
                ],
                [ binary "&&" (BinL And) AssocLeft
                , binary "||" (BinL Or) AssocLeft
                ]
            ]
        binary name fun = Infix  ( reservedOp name >> return fun)
        prefix name fun = Prefix ( reservedOp name >> return fun)

term :: Parser Exp
term =
    parens expression
    <|> (I <$> integer)
    <|> (Id <$> identifier)
    <|> (B <$> boolean)


-- TESTS

test :: IO ()
test = parseTest ifStmt "if ( 1 == 0 ) i = 1 else i = 0"

test1 :: IO ()
test1 = parseTest integer "1"

test2 :: IO ()
test2 = parseTest statement "while (prime && i * i < n) { i = i + 1; if (n % i == 0) prime = false else {} }"

test3 :: IO ()
test3 = parseTest ifStmt "if ( 1 == 0 ) { i = 2; i = 3 } else i = 0"

test4 :: IO ()
test4 = parseTest statement "while (prime && i * i < n) { i = i + 1 }"

test5 :: IO ()
test5 = parseTest statement "print(\"Is prime: \", n)"

test6 :: IO ()
test6 = parseTest statement "{ int i; bool b }"

test7 :: IO ()
test7 = parseTest parseBlock "int i; i = 1; int n; read(\"n=\", n); bool prime; prime = true; while (prime && i * i < n) { i = i + 1; if (n % i == 0) prime = false else {} }; if (prime) print(\"Is prime: \", n) else print(\"Is not prime: \", n)"
