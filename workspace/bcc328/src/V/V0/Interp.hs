module V.V0.Interp where 

import V.V0.Instr 
import Utils.Pretty
import Utils.Value 

type Stack = [Value]

interp :: Code -> Stack -> IO () 
interp [] _ = pure ()
interp (c : cs) stk 
  = do 
      r <- step c stk
      case r of 
        Right stk' -> interp cs stk'
        Left err -> putStrLn err  

step :: Instr -> Stack -> IO (Either String Stack)
step (Push v) stk = pure (Right (v : stk))
step Add (v1 : v2 : stk)
  = case v1 .+. v2 of 
      Left err -> pure (Left err)
      Right v3 -> pure (Right (v3 : stk))
step Mul (v1 : v2 : stk)
  = case v1 .*. v2 of 
      Left err -> pure (Left err) 
      Right v3 -> pure (Right (v3 : stk))
step Print (v : stk)
  = do 
      print (pretty v)
      pure (Right stk)
step Halt stk = pure (Right stk)
step _ _ = pure (Left "Not enough itens on stack!")
