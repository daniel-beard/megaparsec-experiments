module Main where

import Lib

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

import Text.Megaparsec.Pos

-- Grammar for language: 
-- a   ::= x | n | - a | a opa a
-- b   ::= true | false | not b | b opb b | a opr a
-- opa ::= + | - | * | /
-- opb ::= and | or
-- opr ::= > | <

-- Statement Grammar
-- S ::= x := a | skip | S1; S2 | ( S ) | if b then S1 else S2 | while b do S
    
-- Boolean expressions:
data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
  deriving (Show)
  
-- Binary boolean operators
data BBinOp = And | Or deriving (Show)

-- Relational operators:
data RBinOp = Greater | Less deriving (Show)

-- Arithmetic expressions:
data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  deriving (Show)
  
-- Arithmetic operators
data ABinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)
  
-- Statements
data Stmt
  = Seq [Stmt]
  | Assign String AExpr
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  | Skip SourcePos
  deriving (Show)
  
-- Lexer 
----------------------------------------------

-- Space consumer
-- This defines what we consider whitespace in our language
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt 
    where lineCmnt  = L.skipLineComment "//"
          blockCmnt = L.skipBlockComment "/*" "*/"
          
-- We want whitespace to automatically be consumed after each lexeme.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Consume a fixed string then the whitespace after it
symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'parens' parsers something between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'integer' parses an integer
integer :: Parser Integer
integer = lexeme L.integer

-- | 'semi' parses a semicolon.
semi :: Parser String 
semi = symbol ";"

-- Reserved Words
-- Two important notes:
-- Parsers for reserved words should check that the parsed reserved word is NOT a prefix of an identifier
-- Parsers of identifiers should check that parsed identifier is not a reserved word.

-- parse reserved word
rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

-- list of reserved words
rws :: [String]
rws = ["if", "then", "else", "while", "do", "skip", "true", "false", "not", "and", "or"]
    
-- parse identifier
-- First character is a letter, rest is alpha numeric
-- Use of 'try' lets us backtrack to the beginning of the identifier when 'fail' is evaluated.
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
    where p         = (:) <$> letterChar <*> many alphaNumChar
          check x   = if x `elem` rws 
                      then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                      else return x
  
-- Parser
----------------------------------------------------------------------

-- Have to take care of initial whitespace
whileParser :: Parser Stmt
whileParser = between sc eof stmt

-- parse statments
-- Because any statment might be a sequence of statements, we use sepBy1 to parse at least one statement.
-- Result is a list of statments
-- Also allow grouping by parens
stmt :: Parser Stmt
stmt = parens stmt <|> stmtSeq

-- Parse sequence of statments
stmtSeq :: Parser Stmt  
stmtSeq = f <$> sepBy1 stmt' semi
    -- if there's only one stmt return it without using 'Seq'
    where f l = if length l == 1 then head l else Seq l 
        
-- Parse single statement
-- Order is important here!
-- <|> trys a parser, if it fails, it moves on to the next one.
stmt' :: Parser Stmt
stmt' = ifStmt <|> whileStmt <|> skipStmt <|> assignStmt

-- Parse an if statement
ifStmt :: Parser Stmt
ifStmt = do 
    rword "if"
    cond <- bExpr
    rword "then"
    stmt1 <- stmt 
    rword "else"
    stmt2 <- stmt
    return (If cond stmt1 stmt2)
    
-- Parse a while statement
whileStmt :: Parser Stmt
whileStmt = do 
    rword "while"
    cond <- bExpr 
    rword "do"
    stmt1 <- stmt
    return (While cond stmt1)
    
-- Parse a variable assignment statement
assignStmt :: Parser Stmt
assignStmt = do 
    var <- identifier
    void (symbol ":=")
    expr <- aExpr
    return (Assign var expr)
    
-- Parse a skip statement
skipStmt :: Parser Stmt
skipStmt = do -- Skip <$ rword "skip"
    rword "skip"
    sourcePos <- getPosition
    return (Skip sourcePos)

-- Expression Parsing
------------------------------------

-- megaparsec gives us some helpers for defining these.

-- arithmetic expressions
aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

-- binary expressions
bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

-- Specify precendence, associativity and constructors for arithmetic operators
aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/") ]
  , [ InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-") ]
  ]

-- Specify precendence, associativity and constructors for binary operators
bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [Prefix (Not <$ rword "not") ]
  , [InfixL (BBinary And <$ rword "and")
    , InfixL (BBinary Or <$ rword "or") ]
  ]

aTerm :: Parser AExpr
aTerm = parens aExpr
    <|> Var         <$> identifier
    <|> IntConst    <$> integer
    
bTerm :: Parser BExpr
bTerm = parens bExpr
    <|> (rword "true" *> pure (BoolConst True))
    <|> (rword "false" *> pure (BoolConst False))
    <|> rExpr -- relational operator
 
-- relational expressions
rExpr :: Parser BExpr
rExpr = do 
    a1 <- aExpr
    op <- relation 
    a2 <- aExpr 
    return (RBinary op a1 a2)
    
relation :: Parser RBinOp
relation = (symbol ">" *> pure Greater)
    <|> (symbol "<" *> pure Less)

-- Testing notes!
-- Running "parseTest p input" applies parser p on input "input" and prints the results.

main :: IO ()
main = parseTest whileParser "a := 1; skip"
