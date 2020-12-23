-- | Advent of Code 2020 - Day 18

module Day18
    ( day18
    , day18'
    )
where

import           AoC

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Void                      ( Void )
import           Control.Monad.Combinators.Expr

data Expr = Num Int | Add Expr Expr | Mul Expr Expr
  deriving Show

type Parser = Parsec Void String

day18 :: [String] -> Int
day18 = sum . map eval

day18' :: [String] -> Int
day18' = sum . map eval'

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

number :: Parser Int
number = lexeme L.decimal

inParens :: Parser Expr -> Parser Expr
inParens = between (symbol "(") (symbol ")")

term :: Parser Expr
term = choice [inParens expr, Num <$> number]
term' = choice [inParens expr', Num <$> number]

binOp name f = InfixL (symbol name >> return f)

opTable = [[binOp "+" Add, binOp "*" Mul]]
opTable' = [[binOp "+" Add], [binOp "*" Mul]]

expr :: Parser Expr
expr = makeExprParser term opTable <?> "expression"
expr' = makeExprParser term' opTable' <?> "expression"

eval :: String -> Int
eval = evalWith expr
eval' = evalWith expr'

evalWith :: Parser Expr -> String -> Int
evalWith ep s = case parse ep "" s of
    Left  err -> error $ show err
    Right e   -> evalExpr e

evalExpr :: Expr -> Int
evalExpr (Num n  ) = n
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (Mul a b) = evalExpr a * evalExpr b
