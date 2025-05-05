module V.V0.Interp where 

import Data.Map(Map)
import qualified Data.Map as Map

import V.V0.Instr 
import Utils.Pretty
import Utils.Value 
import Utils.Var 

type Stack = [Value]

data Conf 
  = Conf {
      stack :: Stack 
    , memory :: Map Var Value 
    } deriving (Eq, Ord, Show)

push :: Value -> Interp () 
push v (Conf s m) 
  = do 
      Conf (v : s) m

pop :: Conf -> 

type Interp a = Conf -> IO (Either String Conf)

interp :: Code -> Interp () 
interp [] _ = pure ()
interp (c : cs) stk 
  = do 
      r <- step c stk
      case r of 
        Right stk' -> interp cs stk'
        Left err -> putStrLn err  

step :: Instr -> Conf -> IO (Either String Conf)
step (Push v) stk = pure (Right (v : stk))
step Add (v1 : v2 : stk)
  = case v1 .+. v2 of 
      Left err -> pure (Left err)
      Right v3 -> pure (Right (v3 : stk))
step Mul (v1 : v2 : stk)
  = case v1 .*. v2 of 
      Left err -> pure (Left err) 
      Right v3 -> pure (Right (v3 : stk))
step (Input s) 
  = do 
      putStrLn s 
      (VInt . read) <$> getLine 
step Print (v : stk)
  = do 
      print (pretty v)
      pure (Right stk) 
step _ _ = pure (Left "Not enough itens on stack!")
