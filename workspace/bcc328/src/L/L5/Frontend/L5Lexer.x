{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module L.L5.Frontend.L5Lexer where

import Control.Monad
import Utils.Value
}


%wrapper "monadUserState"

-- char macros

$digit = 0-9      -- digits
$lower = [a-z]    -- lower case chars
$upper = [A-Z]    -- upper case chars
$alpha = [a-zA-Z] -- alphabetic characters
$hexdig = [0-9A-Fa-f]

-- second RE macros

@identifier = $alpha[$alpha $digit]* -- identifiers
@number     = $digit+
@hexlit     = 0x$hexdig+

-- tokens declarations

tokens :-
      -- whitespace and line comments
      <0> $white+       ;
      <0> "//"  .*       ;
      -- other tokens
      <0> @number       {mkNumber}
      <0> "("           {simpleToken TLParen}
      <0> ")"           {simpleToken TRParen}
      <0> "program"           {simpleToken TProgram}
      <0> "end"           {simpleToken TEnd}
      <0> "+"           {simpleToken TAdd}
      <0> "-"           {simpleToken TMinus}
      <0> "*"           {simpleToken TMul}
      <0> "/"           {simpleToken TDiv}
      <0> "<"           {simpleToken TLt}
      <0> "="           {simpleToken TEq}
      <0> ":="          {simpleToken TAssign}
      <0> "read"        {simpleToken TRead}
      <0> "true"        {simpleToken TTrue}
      <0> "false"       {simpleToken TFalse}
      <0> "print"       {simpleToken TPrint}
      <0> "strcat"      {simpleToken TCat}
      <0> "strsize"     {simpleToken TSize}
      <0> "if"          {simpleToken TIf}
      <0> "then"        {simpleToken TThen}
      <0> "else"        {simpleToken TElse}
      <0> "while"       {simpleToken TWhile}
      <0> "do"          {simpleToken TDo}
      <0> ";"           {simpleToken TSemi}
      <0> "&&"          {simpleToken TAnd}
      <0> "!"           {simpleToken TNot}
      <0> "let"         {simpleToken TLet}
      <0> ":"           {simpleToken TColon}
      <0> ","           {simpleToken TComma}
      <0> "string"      {simpleToken TStr}
      <0> "int"         {simpleToken TInt}
      <0> "bool"        {simpleToken TBool}
      <0> "i2s"         {simpleToken TI2S}
      <0> "i2b"         {simpleToken TI2B}
      <0> "s2i"         {simpleToken TS2I}
      <0> "s2b"         {simpleToken TS2B}
      <0> "b2s"         {simpleToken TB2S}
      <0> "b2i"         {simpleToken TB2I}
      <0> @identifier   {mkIdentifier}
      -- multi-line comment
      <0> "\*"              { nestComment `andBegin` state_comment }
      <0> "*/"              {\ _ _ -> alexError "Error! Unexpected close comment!" }
      <state_comment> "\*"  { nestComment }
      <state_comment> "*/"  { unnestComment }
      <state_comment> .     ;
      <state_comment> \n    ;
      -- string literals

      <0> \"                                   {enterString `andBegin` state_string}
      <state_string> \\n                       {emit '\n'}
      <state_string> \\t                       {emit '\t'}
      <state_string>  \\\"                     {emit '\"'}
      <state_string>  \"                       {exitString `andBegin` 0}
      <state_string>  .                        {emitCurrent}
{
-- user state

data AlexUserState
  = AlexUserState {
      nestLevel :: Int      -- comment nesting level
    , strStart :: AlexPosn  -- beginning the string
    , strBuffer :: String   -- current string i
    }

alexInitUserState :: AlexUserState
alexInitUserState
  = AlexUserState 0 (AlexPn 0 0 0) []

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f
  = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

-- definition of the EOF token

alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  startCode <- alexGetStartCode
  when (startCode == state_comment) $
    alexError "Error: unclosed comment"
  pure $ Token TEOF (position pos)

-- dealing with comments

nestComment :: AlexAction Token
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len

unnestComment :: AlexAction Token
unnestComment input len
  = do
      s <- get
      let level = (nestLevel s) - 1
      put s{nestLevel = level}
      when (level == 0) $
        alexSetStartCode 0
      skip input len

-- string literals

enterString :: AlexAction Token
enterString inp@(pos, _, _, _) len
  = do
      modify $ \s -> s{ strStart = pos
                      , strBuffer = '"' : strBuffer s
                      }
      skip inp len

exitString :: AlexAction Token
exitString inp@(pos, _, _, _) len
  = do
      s <- get
      put s{strStart = AlexPn 0 0 0, strBuffer = []}
      let tk = TString $ VStr $ reverse $ strBuffer s
      return $ Token tk (position pos)

emit :: Char -> AlexAction Token
emit c inp@(_, _, _, str) len = do
  modify $ \s -> s{strBuffer = c : strBuffer s}
  skip inp len

emitCurrent :: AlexAction Token
emitCurrent (_, _, _, []) _ = alexError "Error: Expecting EOF!"
emitCurrent inp@(_, _, _, (c : _)) len = do
  modify $ \s -> s{strBuffer = c : strBuffer s}
  skip inp len

-- token definition

data Lexeme
  = TLParen
  | TRParen
  | TProgram
  | TNum Value
  | TIdentifier String
  | TAdd
  | TMinus
  | TMul
  | TDiv
  | TLt
  | TEq
  | TCat
  | TSize
  | TRead
  | TString Value
  | TAssign
  | TTrue
  | TFalse
  | TEnd
  | TAnd
  | TNot
  | TI2S
  | TI2B
  | TS2I
  | TS2B
  | TB2I
  | TB2S
  | TSemi
  | TPrint
  | TIf
  | TComma
  | TLet
  | TColon
  | TThen
  | TElse
  | TInt
  | TBool
  | TStr
  | TWhile
  | TDo
  | TEOF
  deriving (Eq, Show)

data Token
  = Token {
      lexeme :: Lexeme
    , pos :: (Int, Int)
    } deriving (Eq, Show)

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

mkNumber :: AlexAction Token
mkNumber (st, _, _, str) len
  = pure $ Token (TNum $ VInt $ read $ take len str) (position st)

mkIdentifier :: AlexAction Token
mkIdentifier (st, _, _, str) len
  = pure $ Token (TIdentifier (take len str)) (position st)

simpleToken :: Lexeme -> AlexAction Token
simpleToken lx (st, _, _, _) _
  = return $ Token lx (position st)

-- lexer main function

lexer :: String -> Either String [Token]
lexer s = runAlex s go
  where
    go = do
      output <- alexMonadScan
      if lexeme output == TEOF then
        pure [output]
      else (output :) <$> go
}
