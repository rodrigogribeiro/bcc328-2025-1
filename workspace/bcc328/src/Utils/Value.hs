module Utils.Value where 

import Utils.Pretty 

data Value 
  = VInt Int
  | VStr String 
    deriving (Eq, Ord, Show)

instance Pretty Value where 
  ppr (VInt n) = int n
  ppr (VStr s) = doubleQuotes (text s)

(.+.) :: Value -> Value -> Either String Value 
(VInt n1) .+. (VInt n2) = Right (VInt (n1 + n2))
e1 .+. e2 = Left $ unwords ["Type error on:", pretty e1, "+", pretty e2] 

(.*.) :: Value -> Value -> Either String Value 
(VInt n1) .*. (VInt n2) = Right (VInt (n1 * n2))
e1 .*. e2 = Left $ unwords ["Type error on:", pretty e1, "*", pretty e2] 
