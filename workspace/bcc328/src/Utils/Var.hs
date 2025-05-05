module Utils.Var where 

import Utils.Pretty 

data Var 
  = Var String deriving (Eq, Ord, Show)

instance Pretty Var where 
  ppr (Var v) = text v
