{
module V.V1.V1Parser where

import Utils.Value
import Utils.Var
import V.V1.V1Lexer hiding (lexer)
import V.V1.Instr
}


%name parser Code
%monad {Alex}{(>>=)}{return}
%tokentype { Token }
%error     { parseError }
%lexer {lexer}{Token TEOF _}


%token
      num       {Token (TNum $$) _}
      ident     {Token (TIdentifier $$) _}
      string    {Token (TString $$) _}
      '('       {Token TLParen _}
      ')'       {Token TRParen _}
      'push'    {Token TPush _}
      'add'     {Token TAdd _}
      'minus'   {Token TMinus _}
      'mul'     {Token TMul _}
      'div'     {Token TDiv _}
      'input'   {Token TInput _}
      'load'    {Token TLoad _}
      'store'   {Token TStore _}
      'print'   {Token TPrint _}
      ';'       {Token TSemi _}
      'halt'    {Token THalt _}

%%

Code :: { Code }
Code : Instr Code    {$1 : $2}
     | {- empty -}  {[]}

Instr :: { Instr }
Instr : 'push' Val ';' {Push $2}
      | 'add'          {Add}
      | 'minus'        {Sub}
      | 'mul'          {Mul}
      | 'div'          {Div}
      | 'input'        {Input}
      | 'print'        {Print}
      | 'load' Var     {Load $2}
      | 'store' Var    {Store $2}
      | 'halt'         {Halt}

Val :: { Value }
Val : num              {$1}
    | string           {$1}

Var :: { Var }
Var : ident            { Var $1 }

{
parserTest :: String -> IO ()
parserTest s = do
  r <- codeParser s
  print r

parseError (Token lexeme (line, col))
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

codeParser :: String -> IO (Either String Code)
codeParser content = do
  pure $ runAlex content parser
}
