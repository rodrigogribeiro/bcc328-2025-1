{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module V.V1.V1Lexer where

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
      <0> "//" .*       ;
      -- other tokens
      <0> @number       {mkNumber}
      <0> "("           {simpleToken TLParen}
      <0> ")"           {simpleToken TRParen}
      <0> "push"        {simpleToken TPush}
      <0> "add"         {simpleToken TAdd}
      <0> "minus"       {simpleToken TMinus}
      <0> "mul"         {simpleToken TMul}
      <0> "div"         {simpleToken TDiv}
      <0> "input"       {simpleToken TInput}
      <0> "load"        {simpleToken TLoad}
      <0> "store"       {simpleToken TStore}
      <0> "print"       {simpleToken TPrint}
      <0> ";"           {simpleToken TSemi}
      <0> "halt"        {simpleToken THalt}
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
  = TPush
  | TLParen
  | TRParen
  | TNum Value
  | TIdentifier String
  | TAdd
  | TMinus
  | TMul
  | TDiv
  | TLt
  | TEq
  | TInput
  | TString Value
  | TLoad
  | TStore
  | TTrue
  | TFalse
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
  | TEOF
  | THalt
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
