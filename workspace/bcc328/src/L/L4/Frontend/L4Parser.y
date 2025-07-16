{
module L.L4.Frontend.L4Parser where

import Utils.Value
import Utils.Var
import L.L4.Frontend.L4Lexer hiding (lexer)
import L.L4.Frontend.Syntax
}


%name parser L4
%monad {Alex}{(>>=)}{return}
%tokentype { Token }
%error     { parseError }
%lexer {lexer}{Token TEOF _}


%token
      num       {Token (TNum $$) _}
      '('       {Token TLParen _}
      ')'       {Token TRParen _}
      '{'       {Token TLBrace _}
      '}'       {Token TRBrace _}
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

L4 :: { L4 }
L4 : B4  {L4 $1}

B4 :: { [S4] }
B4 : '{' S4List '}'         {$2}
   | {- empty -}            {[]}

S4List :: { [S4] }
S4List : S4 S4List          {$1 : $2}
       | {- empty -}        {[]}

S4 :: { S4 }
S4 : 'let' Var ':' T ':=' E4 ';'   {SLet $2 $4 $6}
   | Var ':=' E4 ';'               {SAssign $1 $3}
   | 'read' '(' E4 ',' Var ')' ';' {SRead $3 $5}
   | 'print' '(' E4 ')' ';'        {SPrint $3}
   | 'if' E4 'then' B4 'else' B4   {SIf $2 $4 $6}
   | 'if' E4 'then' B4 %shift      {SIf $2 $4 []}

T :: { Ty }
T : 'string'         {TyString}
  | 'bool'           {TyBool}
  | 'int'            {TyInt}

E4 :: { E4 }
E4 : Val                        {EValue $1}
   | Var                        {EVar $1 Nothing}
   | E4 '+' E4                  {EAdd $1 $3}
   | E4 '-' E4                  {EMinus $1 $3}
   | E4 '*' E4                  {EMult $1 $3}
   | E4 '/' E4                  {EDiv $1 $3}
   | E4 '<' E4                  {ELt $1 $3}
   | E4 '=' E4                  {EEq $1 $3}
   | '-' E4 %prec NEG           {EMinus (EValue (VInt 0)) $2}
   | '!' E4 %prec NOT           {ENot $2}
   | E4 '&&' E4                 {EAnd $1 $3}
   | 'strcat' '(' E4 ',' E4 ')' {ECat $3 $5}
   | 'strsize' '(' E4 ')'       {ESize $3}
   | 'i2s' '(' E4 ')'           {EI2S $3}
   | 'i2b' '(' E4 ')'           {EI2B $3}
   | 's2i' '(' E4 ')'           {ES2I $3}
   | 's2b' '(' E4 ')'           {ES2B $3}
   | 'b2s' '(' E4 ')'           {EB2S $3}
   | 'b2i' '(' E4 ')'           {EB2I $3}

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

codeParser :: String -> IO (Either String L4)
codeParser content = do
  pure $ runAlex content parser
}
