module Parser where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import AST
import Lexer

-- set up table of binary operators with associativity and precedence,
-- so expr can parse expressions automatically
type Operator = Ex.Operator String () Identity Expr

binary :: String -> Op -> Ex.Assoc -> Operator
binary opStr opType assoc = Ex.Infix (reservedOp opStr >> return (BinOp opType)) assoc

opTable :: [[Operator]]
opTable = [
    [ binary "*" Times Ex.AssocLeft
    , binary "/" Divide Ex.AssocLeft
    ],
    [ binary "+" Plus Ex.AssocLeft
    , binary "-" Minus Ex.AssocLeft
    ]]

expr :: Parser Expr
expr = Ex.buildExpressionParser opTable factor

-- factor as in standard Expression-Term-Factor grammar
factor :: Parser Expr
factor =  try floating
      <|> try int
      <|> try extern
      <|> try function
      <|> try call
      <|> variable
      <|> parens expr


      
int :: Parser Expr
int = do
    n <- integer
    return $ Float (fromInteger n)

-- may be able to replace with
-- floating = Float <$> float
floating :: Parser Expr
floating = do
    x <- float
    return $ Float x