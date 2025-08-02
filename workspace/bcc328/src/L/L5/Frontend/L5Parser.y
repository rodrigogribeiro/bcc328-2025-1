{
module L.L5.Frontend.L5Parser where

import Utils.Value
import Utils.Var
import L.L5.Frontend.L5Lexer hiding (lexer)
import L.L5.Frontend.Syntax
}


%name parser L5
%monad {Alex}{(>>=)}{return}
%tokentype { Token }
%error     { parseError }
%lexer {lexer}{Token TEOF _}


%token
      num       {Token (TNum $$) _}
      '('       {Token TLParen _}
      ')'       {Token TRParen _}
      'program' {Token TProgram _}
      'end'     {Token TEnd _}
      '+'       {Token TAdd _}
      '*'       {Token TMul _}
      '-'       {Token TMinus _}
      '/'       {Token TDiv _}
      '='       {Token TEq _}
      ':='      {Token TAssign _}
      'read'    {Token TRead _}
      'true'    {Token TTrue _}
      'false'   {Token TFalse _}
      'print'   {Token TPrint _}
      'strcat'  {Token TCat _}
      'strsize' {Token TSize _}
      'if'      {Token TIf _}
      'then'    {Token TThen _}
      'else'    {Token TElse _}
      'int'     {Token TInt _}
      'bool'    {Token TBool _}
      'string'  {Token TStr _}
      ':'       {Token TColon _}
      'let'     {Token TLet _}
      ','       {Token TComma _}
      ';'       {Token TSemi _}
      '&&'      {Token TAnd _}
      '!'       {Token TNot _}
      ident     {Token (TIdentifier $$) _}
      string    {Token (TString $$) _}
      '<'       {Token TLt _}
      'i2s'     {Token TI2S _}
      'i2b'     {Token TI2B _}
      's2i'     {Token TS2I _}
      's2b'     {Token TS2B _}
      'b2s'     {Token TB2S _}
      'b2i'     {Token TB2I _}

%expect 0

%left '&&'
%nonassoc '<' '='
%left '+' '-'
%left '*' '/'
%left NEG NOT

%%

L5 :: { L5 }
L5 : 'program' B4 'end' {L5 $2}

B4 :: { [S5] }
B4 : S5List             {$1}

S5List :: { [S5] }
S5List : S5 S5List          {$1 : $2}
       | {- empty -}        {[]}

S5 :: { S5 }
S5 : 'let' Var ':' T ':=' E5 ';'   {SLet $2 $4 $6}
   | Var ':=' E5 ';'               {SAssign $1 $3}
   | 'read' '(' E5 ',' Var ')' ';' {SRead $3 $5}
   | 'print' '(' E5 ')' ';'        {SPrint $3}
   | 'if' E5 'then' B4 'else' B4 'end' {SIf $2 $4 $6}
   | 'if' E5 'then' B4 'end' %shift    {SIf $2 $4 []}

T :: { Ty }
T : 'string'         {TyString}
  | 'bool'           {TyBool}
  | 'int'            {TyInt}

E5 :: { E5 }
E5 : Val                        {EValue $1}
   | Var                        {EVar $1 Nothing}
   | E5 '+' E5                  {EAdd $1 $3}
   | E5 '-' E5                  {EMinus $1 $3}
   | E5 '*' E5                  {EMult $1 $3}
   | E5 '/' E5                  {EDiv $1 $3}
   | E5 '<' E5                  {ELt $1 $3}
   | E5 '=' E5                  {EEq $1 $3}
   | '-' E5 %prec NEG           {EMinus (EValue (VInt 0)) $2}
   | '!' E5 %prec NOT           {ENot $2}
   | E5 '&&' E5                 {EAnd $1 $3}
   | 'strcat' '(' E5 ',' E5 ')' {ECat $3 $5}
   | 'strsize' '(' E5 ')'       {ESize $3}
   | 'i2s' '(' E5 ')'           {EI2S $3}
   | 'i2b' '(' E5 ')'           {EI2B $3}
   | 's2i' '(' E5 ')'           {ES2I $3}
   | 's2b' '(' E5 ')'           {ES2B $3}
   | 'b2s' '(' E5 ')'           {EB2S $3}
   | 'b2i' '(' E5 ')'           {EB2I $3}

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
  r <- l5Parser s
  print r

parseError (Token lexeme (line, col))
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

l5Parser :: String -> IO (Either String L5)
l5Parser content = do
  pure $ runAlex content parser
}
