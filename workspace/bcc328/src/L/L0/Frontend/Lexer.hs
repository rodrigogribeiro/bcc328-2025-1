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


{- foldl :: (b -> a -> b) -> b -> [a] -> b 
 - foldl _ v [] = v 
 - foldl f v (x : xs) = foldl f (f v x) xs
 -
 -
 - lexer "12" = 
 - either Left (Right . extract) (foldl step (Right (1,1,"",[])) "12") = 
 - either Left (Right . extract) (foldl step (step (Right (1,1,"", [])) '1') "2") =
 - either Left (Right . extract) (foldl step (transition (Right (1,1,"", [])) '1') "2") =
 - either Left (Right . extract) (foldl step (transition (1,1,"", []) '1') "2") =
 - either Left (Right . extract) (foldl step (Right (1,2,"1", [])) "2") =
 - either Left (Right . extract) (foldl step (step (Right (1,2,"1", [])) '2') "") =
 - either Left (Right . extract) (foldl step (transition (1,2,"1", []) '2') "") =
 - either Left (Right . extract) (foldl step (Right (1,3,"21", [])) "") =
 - either Left (Right . extract) (Right (1,3,"21", [])) = 
 - Right (extract (1,3, "21", []))
 - Right [Token (TNumber (VInt 12) (1,1))]
 - -}

 {-
  - A -> 0 A 1 | 1 
  -
  - A () {
  -    t <- nextToken 
  -    if t == 0 then 
  -       consume(0)
  -       A () 
  -       consume(1)
  -    else consume(1)
  - }
  -
  - B -> B0 | 1 
  -
  - B () {
  -    B ()
  -    t <- nextToken 
  -    if t == 0 then 
  -       consume (0)
  -    consume(1)
  -
  - }
  - -}


lexer :: String -> Either String [Token]
lexer = either Left (Right . extract) . foldl step (Right (1, 1, "", []))
  where 
    step ac@(Left _) _ = ac 
    step (Right state) c = transition state c  

    extract (l, col, s, ts) 
      | null s = reverse ts 
      | otherwise = let t = Token (TNumber (VInt (read $ reverse s))) (l, col - (length s))
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

