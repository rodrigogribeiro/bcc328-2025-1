module L.L1.Interpreter.Interp where 

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map 

import L.L1.Frontend.Syntax

import Utils.Pretty
import Utils.Value 
import Utils.Var 

type Env = Map Var Value 

evalL1 :: L1 -> IO (Either String Env) 
evalL1 (L1 ss)
  = foldM step (Right Map.empty) ss 
  where 
    step ac@(Left _) _ = pure ac 
    step (Right env) s1 = evalS1 env s1 
      

evalS1 :: Env -> S1 -> IO (Either String Env) 
evalS1 env (LRead s v)
  = do 
      putStr s 
      val <- readValue
      pure (Right $ Map.insert v val env)
evalS1 env (LPrint e)
  = case evalE1 env e of 
      Left err -> pure $ Left err 
      Right val -> do 
        putStrLn (pretty val)
        pure (Right env) 
evalS1 env (LAssign v e)
  = case evalE1 env e of 
      Left err -> pure $ Left err 
      Right val -> pure (Right $ Map.insert v val env)


readValue :: IO Value 
readValue = (VInt . read) <$> getLine 

evalE1 :: Env -> E1 -> Either String Value 
evalE1 _ (LVal v) = Right v 
evalE1 env (LVar v) 
  = case Map.lookup v env of 
      Just val -> Right val 
      Nothing -> Left ("Undefined variable: " ++ pretty v)
evalE1 env (LAdd l1 l2) 
  = do 
      v1 <- evalE1 env l1
      v2 <- evalE1 env l2 
      v1 .+. v2
evalE1 env (LMul l1 l2) 
  = do 
      v1 <- evalE1 env l1
      v2 <- evalE1 env l2
      v1 .*. v2
