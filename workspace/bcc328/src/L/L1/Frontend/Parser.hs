module L.L1.Frontend.Parser where 

import Control.Applicative

import L.L0.Frontend.Lexer 
import L.L0.Frontend.Syntax 
import qualified Utils.Parser as P
import Utils.Parser (sat, chainl, runParser)
import Utils.Value

type Parser a = P.Parser Token a 

-- constructing the parser 

l0Parser :: [Token] -> Either String L0 
l0Parser tks 
  = case runParser expParser tks of 
      ((t, []) : _) -> Right t 
      _             -> Left "Parse error on program."

expParser :: Parser L0 
expParser 
  = chainl padd termParser
    where 
      padd = (const LAdd) <$> sat (\ t -> lexeme t == TPlus)

termParser :: Parser L0 
termParser 
  = chainl pmult factorParser
    where  
      pmult = (const LMul) <$> sat (\ t -> lexeme t == TMul)

factorParser :: Parser L0
factorParser 
  = (LVal <$> valueParser) <|> parens expParser 

valueParser :: Parser Value
valueParser = f <$> sat (\ t -> case lexeme t of 
                                  TNumber _ -> True 
                                  _ -> False)
      where 
        f (Token (TNumber n) _) = n 
        f _ = error "Impossible! Parser.valueParser"


parens :: Parser a -> Parser a 
parens p 
  = f <$> lparen <*> p <*> rparen 
    where 
      f _ x _ = x 

lparen :: Parser Token 
lparen = sat (\ t -> lexeme t == TLParen)

rparen :: Parser Token 
rparen = sat (\ t -> lexeme t == TRParen)


