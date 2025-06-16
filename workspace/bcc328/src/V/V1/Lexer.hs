module V.V1.Lexer where 

import Data.Char
import Utils.Value 

data Lexeme 
  = TPush
  | TNum Value  
  | TAdd 
  | TMul
  | TSemi
  | TPrint 
  | THalt 
  deriving (Eq, Show)

data Token 
  = Token {
      lexeme :: Lexeme 
    , position :: (Int, Int)
    } deriving (Eq, Show)

-- hand written lexer 

type Line = Int 
type Column = Int
type State = (Line, Column, String, [Token])

lexer :: String -> Either String [Token]
lexer = either Left (Right . extract) . foldl step (Right (1, 1, "", []))
  where 
    step ac@(Left _) _ = ac 
    step (Right state) c = transition state c  

    extract (l, col, s, ts) 
      | null s = reverse ts 
      | otherwise = let t = Token (TNum (VInt (read $ reverse s))) (l, col) 
                    in reverse (t : ts)

transition :: State -> Char -> Either String State
transition state@(l, col, t, ts) c 
  | c == '\n' = case mkToken state of 
                  Left err -> Left err 
                  Right tk -> Right (l + 1, col, "", tk : ts) 
  | isSpace c = case mkToken state of 
                  Left err -> Left err 
                  Right tk -> Right (l, col + 1, "", tk : ts)
  | otherwise = Right (l, col + 1, c : t, ts) 


mkToken :: State -> Either String Token 
mkToken (_, _, [], _) = Left $ "unexpected EOF" 
mkToken (l, col, s@(c : _), _)
  | c == ';' = mk l col TSemi
  | all isDigit s = mk l col (TNum (VInt (read $ reverse s)))
  | s == "add" = mk l col TAdd 
  | s == "mul" = mk l col TMul 
  | s == "print" = mk l col TPrint 
  | s == "halt" = mk l col THalt 
  | s == "push" = mk l col TPush 
  | otherwise = unexpectedCharError l col c 

mk :: Line -> Column -> Lexeme -> Either String Token 
mk l c x = Right $ Token x (l,c)

unexpectedCharError :: Line -> Column -> Char -> Either String Token
unexpectedCharError l col c 
  = Left (unwords [ "Unexpected character at line"
                              , show l 
                              , "column"
                              , show col 
                              , ":"
                              , [c]
                              ])

