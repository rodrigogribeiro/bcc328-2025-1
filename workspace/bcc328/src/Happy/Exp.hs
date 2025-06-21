module Happy.Exp where 

-- abstract syntax tree for simple expressions 

data Exp 
  = Const Int 
  | Add Exp Exp 
  | Mul Exp Exp 
  deriving (Eq, Ord, Show)
