{
module L.L7.Frontend.L7Parser where

import Utils.Value
import Utils.Var
import L.L7.Frontend.L7Lexer hiding (lexer)
import L.L7.Frontend.Syntax
}


%name parser L7
%monad {Alex}{(>>=)}{return}
%tokentype { Token }
%error     { parseError }
%lexer {lexer}{Token TEOF _}


%token
      num       {Token (TNum $$) _}
      '('       {Token TLParen _}
      ')'       {Token TRParen _}
      'fun'     {Token TFun _}
      '->'      {Token TArrow _}
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
      'unit'    {Token TUnit _}
      'return'  {Token TReturn _}
      'while'   {Token TWhile _}
      'do'      {Token TDo _}
      ':'       {Token TColon _}
      'let'     {Token TLet _}
      ','       {Token TComma _}
      ';'       {Token TSemi _}
      '&&'      {Token TAnd _}
      '!'       {Token TNot _}
      '<'       {Token TLt _}
      'i2s'     {Token TI2S _}
      'i2b'     {Token TI2B _}
      's2i'     {Token TS2I _}
      's2b'     {Token TS2B _}
      'b2s'     {Token TB2S _}
      'b2i'     {Token TB2I _}
      ident     {Token (TIdentifier $$) _}
      string    {Token (TString $$) _}
%expect 0

%left '&&'
%nonassoc '<' '='
%left '+' '-'
%left '*' '/'
%left NEG NOT

%%

L7 :: { L7 }
L7 : D7List {L7 $1}

D7List :: { [D7] }
D7List : D7 D7List       {$1 : $2}
       | {- empty -}     {[]}

D7 :: {D7}
D7 : 'fun' Var '(' ParamList ')' '->' T 'do' S7List 'end' {Fun $2 $4 $7 $9}

ParamList :: {[Param]}
ParamList : Param               {[$1]}
          | Param ',' ParamList {$1 : $3}
          | {- empty -}         {[]}

Param :: {Param}
Param : Var ':' T               {Param $1 $3}

B7 :: {[S7]}
B7 : S7List                 {$1}

S7List :: { [S7] }
S7List : S7 S7List          {$1 : $2}
       | {- empty -}        {[]}

S7 :: { S7 }
S7 : 'let' Var ':' T ':=' E7 ';'   {SLet $2 $4 $6}
   | Var ':=' E7 ';'               {SAssign $1 $3}
   | 'read' '(' E7 ',' Var ')' ';' {SRead $3 $5}
   | 'print' '(' E7 ')' ';'        {SPrint $3}
   | 'if' E7 'then' B7 'else' B7 'end' {SIf $2 $4 $6}
   | 'if' E7 'then' B7 'end' %shift    {SIf $2 $4 []}
   | 'while' E7 'do' B7 'end'          {SWhile $2 $4}
   | 'return' E7 ';'                   {SReturn $2}

T :: { Ty }
T : 'string'         {TyString}
  | 'bool'           {TyBool}
  | 'int'            {TyInt}
  | 'unit'           {TyUnit}

E7 :: { E7 }
E7 : Val                        {EValue $1}
   | Var                        {EVar $1 Nothing}
   | E7 '+' E7                  {EAdd $1 $3}
   | E7 '-' E7                  {EMinus $1 $3}
   | E7 '*' E7                  {EMult $1 $3}
   | E7 '/' E7                  {EDiv $1 $3}
   | E7 '<' E7                  {ELt $1 $3}
   | E7 '=' E7                  {EEq $1 $3}
   | '-' E7 %prec NEG           {EMinus (EValue (VInt 0)) $2}
   | '!' E7 %prec NOT           {ENot $2}
   | E7 '&&' E7                 {EAnd $1 $3}
   | 'strcat' '(' E7 ',' E7 ')' {ECat $3 $5}
   | 'strsize' '(' E7 ')'       {ESize $3}
   | 'i2s' '(' E7 ')'           {EI2S $3}
   | 'i2b' '(' E7 ')'           {EI2B $3}
   | 's2i' '(' E7 ')'           {ES2I $3}
   | 's2b' '(' E7 ')'           {ES2B $3}
   | 'b2s' '(' E7 ')'           {EB2S $3}
   | 'b2i' '(' E7 ')'           {EB2I $3}
   | Var '(' ArgList ')'        {SCall $1 $3}

ArgList :: {[E7]}
ArgList : E7                    {[$1]}
        | E7 ',' ArgList        {$1 : $3}
        | {- empty -}           {[]}

Val :: { Value }
Val : num              {$1}
    | string           {$1}
    | 'true'           {VBool True}
    | 'false'          {VBool False}
    | 'unit'           {VUnit}

Var :: { Var }
Var : ident            { Var $1 }

{
parserTest :: String -> IO ()
parserTest s = do
  r <- l7Parser s
  print r

parseError (Token lexeme (line, col))
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

l7Parser :: String -> IO (Either String L7)
l7Parser content = do
  pure $ runAlex content parser
}
