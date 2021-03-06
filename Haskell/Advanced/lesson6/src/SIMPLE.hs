module SIMPLE where

type Name = String

data BinAop = Add | Mul | Sub | Div | Mod

instance Show BinAop where
    show Add = "+"
    show Mul = "*"
    show Sub = "-"
    show Div = "/"
    show Mod = "%"

data BinCop = Lt | Lte | Gt | Gte

instance Show BinCop where
    show Lt = "<"
    show Lte = "<="
    show Gt = ">"
    show Gte = ">="

data BinEop = Eq | Neq

instance Show  BinEop where
    show Eq = "=="
    show Neq = "!="

data BinLop = And | Or

instance Show BinLop where
    show And = "&&"
    show Or = "||"

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

instance Show Exp where
    show (Id x) = x
    show (I i) = show i
    show (B True) = "true"
    show (B False) = "false"
    show (UMin e) = "-" <> show e
    show (BinA op e1 e2) = addParens $ show e1 <> show op <> show e2
    show (BinC op e1 e2) = show e1 <> show op <> show e2
    show (BinE op e1 e2) = show e1 <> show op <> show e2
    show (BinL op e1 e2) = addParens $ show e1 <> show op <> show e2
    show (Not e) = "!" <> show e

addParens :: String -> String
addParens e = "(" <> e <> ")"


data Stmt
    = Asgn Name Exp
    | If Exp Stmt Stmt
    | Read String Name
    | Print String Exp
    | While Exp Stmt
    | Block [Stmt]
    | Decl Name Exp
    deriving (Show)


pFact = Block [ 
        Decl "n" (I 5),
        Decl "fact" (Id "n"),
        Decl "i" (I 1),
        While (BinE Neq  (Id "n") (Id "i")) 
                (Block  [ Asgn "fact" (BinA Mul (Id "fact") (Id "i")),
                        Asgn "i" (BinA Add (Id "i") (I 1))
                        ])
            ]

pPrime = Block [
            Decl "prime" (B True),
            Decl "i" (I 0),
            Decl "n" (I 0),
            While (BinL And 
                        (Id "prime") 
                        (BinC Lt (BinA Mul (Id "i") (Id "i")) (Id "n"))) 
                (Block [
                    Asgn "i" (BinA Add (Id "i") (I 1)),
                    If (BinE Eq (BinA Mod (Id "n") (Id "i")) (I 0)) 
                        (Asgn "prime" (B False)) 
                        (Block [])]
                )
    ]
