module L.L0.Interpreter.Interp where 

import L.L0.Frontend.Lexer 
import L.L0.Frontend.Parser 
import L.L0.Frontend.Syntax
import Utils.Value 

interp :: String -> Either String Value 
interp content 
  = do 
      tokens <- lexer content
      ast <- l0Parser tokens
      eval ast

eval :: L0 -> Either String Value 
eval (LVal v) = Right v 
eval (LAdd l1 l2) 
  = do 
       v1 <- eval l1 
       v2 <- eval l2 
       v1 .+. v2 
eval (LMul l1 l2) 
  = do 
       v1 <- eval l1
       v2 <- eval l2 
       v1 .*. v2
