module L.L0.Frontend.Lexer where 

import Data.Char
import Utils.Value 

-- definition of the token type 

data Token 
  = Token {
      lexeme :: Lexeme
    , position :: (Int, Int)
    } deriving (Eq, Ord, Show)

data Lexeme  
  = TNumber Value  
  | TPlus 
  | TMul
  | TLParen
  | TRParen 
  deriving (Eq, Ord, Show)

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
      | otherwise = let t = Token (TNumber (VInt (read $ reverse s))) (l, col)
                    in reverse (t : ts)

transition :: State -> Char -> Either String State
transition state@(l, col, t, ts) c 
  | c == '\n' = mkDigits state c 
  | isSpace c = mkDigits state c 
  | c == '+' = Right (l, col + 1, "", mkToken state (Token TPlus (l,col)) ++ ts)
  | c == '*' = Right (l, col + 1, "", mkToken state (Token TMul (l,col)) ++ ts)
  | c == '(' = Right (l, col + 1, "", mkToken state (Token TLParen (l,col)) ++ ts)
  | c == ')' = Right (l, col + 1, "", mkToken state (Token TRParen (l,col)) ++ ts)
  | isDigit c = Right (l, col + 1, c : t, ts)
  | otherwise = unexpectedCharError l col c 

mkToken :: State -> Token -> [Token]
mkToken (l,c, s@(_ : _), _) t 
  | all isDigit s = [t, Token (TNumber (VInt (read $ reverse s))) (l,c)]
  | otherwise = [t]
mkToken _ t = [t] 

mkDigits :: State -> Char -> Either String State  
mkDigits state@(l, col, s, ts) c 
  | null s = Right state 
  | all isDigit s = let t = Token (TNumber (VInt (read $ reverse s))) (l,col)
                        l' = if c == '\n' then l + 1 else l 
                        col' = if c /= '\n' && isSpace c then col + 1 else col  
                    in Right (l', col', "", t : ts)
  | otherwise = unexpectedCharError l col c 



unexpectedCharError :: Line -> Column -> Char -> Either String State
unexpectedCharError l col c 
  = Left (unwords [ "Unexpected character at line"
                              , show l 
                              , "column"
                              , show col 
                              , ":"
                              , [c]
                              ])

