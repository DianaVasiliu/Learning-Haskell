module Parsing where

import Data.Char
import Data.List (intercalate)
import Control.Applicative

newtype Parser a = Parser { apply :: String -> [(a, String)] }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
    where
    f []                 = []
    f (c:s) | p c        = [(c, s)]
            | otherwise = []

parse :: Show a => Parser a -> String -> a
parse m s = case parses of
        [] -> error "No valid parse."
        [a] -> a
        l -> error ("Ambiguity. Possible parses: \n\t" ++ intercalate "\n\t" (map show l))
    where
        parses = [ x | (x,t) <- apply m s, t == "" ]

instance Functor Parser where
    fmap f p =
        Parser
        (\s -> [(f a, r) | (a, r) <- apply p s])

instance Applicative Parser where
    pure a = Parser (\s -> [(a, s)])
    pf <*> pa =
        Parser
            (\s -> [(f a, r) | (f, rf) <- apply pf s
                            , (a, r) <- apply pa rf ])

instance Alternative Parser where
    empty = Parser (\s -> [])
    pa <|> pb = Parser (\s -> apply pa s ++ apply pb s)

-- Recognizing a character
char :: Char -> Parser Char
char c = satisfy (== c)

skipSpace :: Parser ()
skipSpace = many (satisfy isSpace) *> pure ()

token :: Parser a -> Parser a
token p = skipSpace *> p <* skipSpace

parseNat :: Parser Int
parseNat = read <$> some (satisfy isDigit)

-- Recognizing a negative number
parseNeg :: Parser Int
parseNeg =  char '-' *> (negate <$> parseNat)

-- Recognizing an integer number
parseInt :: Parser Int
parseInt = parseNat <|> parseNeg
