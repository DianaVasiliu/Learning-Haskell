# Lesson 5

## Parsing the programs using `Parsec`

[Parsec](https://hackage.haskell.org/package/parsec) is a library for lexical and syntactic analysis, one of many available for the Haskell language.

Parsec implements complex functionalities, of which we will explore today those related to building a parser for a language.

## Lexical analysis

To make parsing more efficient, Parsec offers the possibility to define and use a lexical analyzer (tokenizer) for our language.

The initialization of the lexical analyzer is done by specifying several generic parameters of the language, as members of a record.

The parameters are as follows:

-   `commentStart` - the start of a comment block

-   `commentEnd` - the end of a comment block
-   `commentLine` - the start of a comment line
-   `nestedComments` - if nested comments are allowed
-   `identStart` - parser for the first character of an identifier
-   `identLetter` - parser for the next character of an identifier
-   `opStart` - parser for the first character of an operator
-   `opLetter` - parser for the next character of an operator
-   `reservedNames` - the list of the keywords
-   `reservedOpNames` - the list of the predefined operators
-   `caseSensitive` - if the language makes difference between upper and lower case

For example, for `IMP` language, an initialization could look like this:

```haskell
import qualified Text.Parsec.Token as Token
import Text.Parsec.String ( Parser, parseFromFile )
import Text.Parsec.Expr
    ( buildExpressionParser,
    Assoc(..),
    Operator(..) )
import Text.ParserCombinators.Parsec.Language
    ( emptyDef,
    GenLanguageDef( .. ),
    LanguageDef)
import Text.Parsec ( alphaNum, letter, (<|>), eof )

impLanguageDef :: LanguageDef ()

impLanguageDef =
    emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , nestedComments = False
    , caseSensitive = True
    , identStart = letter
    , identLetter = alphaNum
    , reservedNames =
        [ "while", "if", "else", "int", "bool"
        , "true", "false", "read", "print"
        ]
    , reservedOpNames =
        [ "+", "-", "*", "/", "%"
        , "==", "!=", "<", "<=", ">=", ">"
        , "&&", "||", "!", "="
        ]
    }
```

Based on such a "language definition" we can create a lexical analyzer.

```haskell
impLexer :: Token.TokenParser ()
impLexer = Token.makeTokenParser impLanguageDef
```

This `TokenParser` is in turn a record type, which gives us several parsers built on the definition of language given by us.

For example:

-   `identifier` - parser for identifiers
-   `reserved` - parser for keywords
-   `reservedOp` - parser for predefined operators
-   `integer` - parser for integers
-   `whiteSpace` - parser for unimportant spaces
-   `parens` - parser for something between parenthesis
-   `braces` - parser for something between braces
-   `semiSep` - parser for lists separated by `;`

To make it easier to work with them, without always having to specify the basic `impLexer` record, it is common to use local definitions:

```haskell
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

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace impLexer
```

For example, using these constructs, we can describe a parser for the `if (exp) s1 else s2` statement as follows:

```haskell
ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    cond <- parens expression
    thenS <- statement
    reserved "else"
    elseS <- statement
    return (If cond thenS elseS)

statement :: Parser Stmt
statement = ifStmt <|> ....

expression :: Parser Exp
expression = ...
```

The parser thus obtained will know how to skip spaces and comments, to recognize keywords.

## Recognition of expressions

The difficulty in recognizing expressions is that arithmetic and logic operators have certain priorities and their own ways of grouping.

Although it would not be impossible to build a parser on your own that takes into account all these properties, the process would be difficult and the result quite complex.

Fortunately, Parsec comes to the aid, through the `Text.Parsec.Expr` module, which provides ways to specify the attributes of operations and their precedence as well as a combiner to build a parser for expressions from a table of operators and a parser for atomic expressions (identifiers, constant).

For example, for the `IMP` language, the expression parser might look like this:

```haskell
expression :: Parser Exp
expression = buildExpressionParser operators term
    where
        operators =
            [ [ prefix "!" Not
              ]
            , [ binary "*" (BinA Mul) AssocLeft
              ]
            , [ binary "+" (BinA Add) AssocLeft
              ]
            , [ binary "==" (BinE Eq) AssocNone
            ,   binary "<=" (BinC Lte) AssocNone
              ]
            , [ binary "&&" (BinL And) AssocLeft
            ,   binary "||" (BinL Or) AssocLeft
              ]
            ]
        binary name fun = Infix ( reservedOp name >> return fun)
        prefix name fun = Prefix ( reservedOp name >> return fun)
```

where the atomic expression parser can be defined as:

```haskell
term :: Parser Exp
term =
    parens expression
    <|> (I <$> integer)
    <|> (Id <$> identifier)
```

In the example above, operators is a list of operator lists, specifying in order the precedence groups of operators in the language.

One group before another indicates a higher precedence. Operators in the same group have the same precedence. `AssocLeft` indicates that the grouping is to the left.

Constructors, such as `Not` or `BinA Mul` are the functions to which we want to transmit the parsed values of the operands corresponding to the specified operator.

## Exercise

Write a parser for `IMP`, which should turn a program like the one contained in the `1.imp` file into the abstract syntax contained in the `imp.hs` file.
