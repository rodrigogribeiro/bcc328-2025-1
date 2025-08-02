{
module Happy.ParserExample (expParser, parserTest) where

import Alex.LexerExample hiding (lexer)
import Happy.Exp
}


%name parser Exp
%monad {Alex}{(>>=)}{return}
%tokentype { Token }
%error     { parseError }
%lexer {lexer}{Token _ TEOF}


%token
      num       {Token _ (TNumber $$)}
      '('       {Token _ TLParen}
      ')'       {Token _ TRParen}
      '+'       {Token _ TPlus}
      '*'       {Token _ TTimes}

%left '+'
%left '*'

%%

Exp : num         { Const $1 }
    | Exp '+' Exp { Add $1 $3 }
    | Exp '*' Exp { Mul $1 $3 }
    | '(' Exp ')' { $2 }

{
parserTest :: String -> IO ()
parserTest s = do
  r <- expParser s
  print r

parseError (Token (line, col) lexeme)
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

expParser :: String -> IO (Either String Exp)
expParser content = do
  pure $ runAlex content parser
}
