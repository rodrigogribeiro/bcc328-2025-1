{
module V.V2.V2Parser where

import Utils.Value
import Utils.Var
import V.V2.V2Lexer hiding (lexer)
import V.V2.Instr
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
      'push'    {Token TPush _}
      'add'     {Token TAdd _}
      'minus'   {Token TMinus _}
      'mul'     {Token TMul _}
      'div'     {Token TDiv _}
      'lt'      {Token TLt _}
      'cat'     {Token TCat _}
      'size'    {Token TSize _}
      'eq'      {Token TEq _}
      'input'   {Token TInput _}
      'load'    {Token TLoad _}
      'store'   {Token TStore _}
      'true'    {Token TTrue _}
      'false'   {Token TFalse _}
      'print'   {Token TPrint _}
      ';'       {Token TSemi _}
      'and'     {Token TAnd _}
      'not'     {Token TNot _}
      'i2s'     {Token TI2S _}
      'i2b'     {Token TI2B _}
      's2i'     {Token TS2I _}
      's2b'     {Token TS2B _}
      'b2s'     {Token TB2S _}
      'b2i'     {Token TB2I _}
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
      | 'lt'           {Lt}
      | 'eq'           {IEq}
      | 'and'          {And}
      | 'not'          {Not}
      | 'cat'          {Cat}
      | 'size'         {Size}
      | 'i2s'          {I2S}
      | 'i2b'          {I2B}
      | 'b2s'          {B2S}
      | 'b2i'          {B2I}
      | 's2i'          {S2I}
      | 's2b'          {S2B}
      | 'input'        {Input}
      | 'print'        {Print}
      | 'load' Var     {Load $2}
      | 'store' Var    {Store $2}
      | 'halt'         {Halt}

Val :: { Value }
Val : num              {$1}
    | string           {$1}
    | 'true'           {VBool True}
    | 'false'          {VBool False}

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
