module Megaparsec.ParserExample where

import Control.Monad.Combinators.Expr

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


-- definition of a type for parsers

type Parser = Parsec Void String

-- definition of a type for parser errors

type ParserError = ParseErrorBundle String Void

-- consuming whitespace and comments

slexer :: Parser ()
slexer = L.space space1
                 (L.skipLineComment "//")
                 (L.skipBlockComment "/*" "*/")

-- parsing a symbol

symbol :: String -> Parser String
symbol s = L.symbol slexer s

-- dealing with parenthesis

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- producing a lexeme

lexeme :: Parser a -> Parser a
lexeme = L.lexeme slexer

-- parsing a number

integer :: Parser Int
integer = lexeme L.decimal

-- definition of the AST

data Exp
  = Const Int
  | Add Exp Exp
  | Mul Exp Exp
  deriving Show

-- definition of the parser

pFactor :: Parser Exp
pFactor = choice [ Const <$> integer
                 , parens pExp
                 ]

pExp :: Parser Exp
pExp = makeExprParser pFactor optable

binary :: String -> (Exp -> Exp -> Exp) -> Operator Parser Exp
binary name f = InfixL (f <$ symbol name)

optable :: [[Operator Parser Exp]]
optable = [
            [binary "*" Mul]
          , [binary "+" Add]
          ]
