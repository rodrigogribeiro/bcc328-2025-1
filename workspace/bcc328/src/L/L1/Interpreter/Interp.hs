module L.L1.Interpreter.Interp where 

import Data.Map (Map)
import qualified Data.Map as Map 

import L.L1.Frontend.Syntax

import Utils.Pretty
import Utils.Value 
import Utils.Var 

type Env = Map Var Value 

eval :: Env -> E1 -> Either String Value 
eval _ (LVal v) = Right v 
eval env (LVar v) 
  = case Map.lookup v env of 
      Just val -> Right val 
      Nothing -> Left ("Undefined variable: " ++ pretty v)
eval env (LAdd l1 l2) 
  = do 
      v1 <- eval env l1
      v2 <- eval env l2 
      v1 .+. v2
eval env (LMul l1 l2) 
  = do 
      v1 <- eval env l1
      v2 <- eval env l2
      v1 .*. v2
