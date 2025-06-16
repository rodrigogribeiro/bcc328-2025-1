module L.L1.Frontend.Parser where 

import Control.Applicative

import L.L1.Frontend.Syntax 

import Control.Monad.Combinators.Expr

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


-- definition of a type for parsers

type Parser = Parsec Void String

-- definition of a type for parser errors

type ParserError = ParseErrorBundle String Void


l1Parser :: String -> Either String L1 
l1Parser = error "Not implemented"
