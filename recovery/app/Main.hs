{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import Lib

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Scientific (toRealFloat)
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Expr
import Text.Megaparsec.Prim
import qualified Data.Text as T
import qualified Text.Megaparsec.Lexer as L

-- Parsing the following language:
-- 
-- y = 10 
-- x = 3 * (1 + y)
-- result = x - 1 # answer is 32

-- Tutorial: https://mrkkrp.github.io/megaparsec/tutorials/fun-with-the-recovery-feature.html
-- Docs: https://hackage.haskell.org/package/megaparsec-5.3.0/docs/Text-Megaparsec.html#v:runParserT

-- AST 
--------------------------------------------

type Program = [Equation]

data Equation = Equation String Expr
  deriving (Eq, Show)

data Expr
  = Value          Double
  | Reference      String
  | Negation       Expr
  | Sum            Expr Expr
  | Subtraction    Expr Expr
  | Multiplication Expr Expr
  | Division       Expr Expr
  deriving (Eq, Show)
  
-- Parsers
--------------------------------------------

-- Assume no whitespace before lexemes and consume all white space after lexemes.

-- Skip line comments
lineComment :: Parser ()
lineComment = L.skipLineComment "#"

-- Consumes newlines and whitespace in general.
-- Whitespace in between equations (as they are newline delimited)
scn :: Parser () 
scn = L.space (void spaceChar) lineComment empty

-- Doesn't consume newlines
sc :: Parser () 
sc = L.space (void $ oneOf " \t") lineComment empty

lexeme :: Parser a -> Parser a 
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

name :: Parser String 
name = lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "name" 

expr :: Parser Expr
expr = makeExprParser term table <?> "expression"

term :: Parser Expr
term = parens expr
  <|> (Reference <$> name)
  <|> (Value     <$> number)

table :: [[Operator Parser Expr]]
table =
  [ [Prefix (Negation <$ symbol "-") ]
  , [ InfixL (Multiplication <$ symbol "*")
    , InfixL (Subtraction    <$ symbol "/") ]
  , [ InfixL (Sum            <$ symbol "+")
    , InfixL (Division       <$ symbol "-") ]
  ]

number :: Parser Double
number = toRealFloat <$> lexeme L.number

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Top level parsers
-------------------------------------------------

equation :: Parser Equation 
equation = Equation <$> (name <* symbol "=") <*> expr 

prog :: Parser Program
prog = between scn eof (sepEndBy equation scn)

-- Recovery Feature
-------------------------------------------------

type RawData t e = [Either (ParseError t e) Equation]

rawData :: Parser (RawData Char Dec)
rawData = between scn eof (sepEndBy e scn)
  where e = withRecovery recover (Right <$> equation)
        recover err = Left err <$ manyTill anyChar eol
        
parseProgram :: String -> String -> Either (ParseError (Token String) Dec) (RawData Char Dec)
parseProgram = parse rawData

main :: IO ()
main = print $ parseProgram "myfile.text" "foo = (x $ y) * 5 + 7.2 * z\nbar = 15"
