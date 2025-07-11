module V.V0.Parser where

import Utils.Parser
import V.V0.Instr
import V.V0.Lexer

codeParser :: [Token] -> Either String Code
codeParser tks
  = case runParser codeParser' tks of
      ((ast, []) : _) -> Right ast
      _ -> Left "Parser error"

codeParser' :: Parser Token Code
codeParser' = instrParser `endBy` sat (\ t -> lexeme t == TSemi)

instrParser :: Parser Token Instr
instrParser
  = choice [ addParser
           , mulParser
           , printParser
           , haltParser
           , pushParser
           ] <* semiParser

addParser :: Parser Token Instr
addParser = const Add <$> sat (\ t -> lexeme t == TAdd)

mulParser :: Parser Token Instr
mulParser = const Mul <$> sat (\ t -> lexeme t == TMul)

printParser :: Parser Token Instr
printParser = const Print <$> sat (\ t -> lexeme t == TPrint)

haltParser :: Parser Token Instr
haltParser = const Halt <$> sat (\ t -> lexeme t == THalt)

pushParser :: Parser Token Instr
pushParser
  = f <$> sat (\ t -> lexeme t == TPush) <*>
          sat (\ t -> isNum (lexeme t))
    where
      isNum (TNum _) = True
      isNum _ = False

      f _ (Token (TNum n) _) = Push n
      f _ _ = error "Impossible! Should result in a parser error!"

semiParser :: Parser Token ()
semiParser = const () <$> sat (\ t -> lexeme t == TSemi)


