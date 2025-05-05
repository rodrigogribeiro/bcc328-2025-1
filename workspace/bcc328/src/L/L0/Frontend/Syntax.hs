module L.L0.Frontend.Syntax where 

import Utils.Pretty 
import Utils.Value

-- definition of the syntax of L0 
-- programs

data L0 
  = LVal Value  
  | LAdd L0 L0 
  | LMul L0 L0 
  deriving (Eq, Ord, Show)

instance Pretty L0 where 
  ppr = pprAdd


pprAdd :: L0 -> Doc
pprAdd (LAdd e1 e2)
  = hsep [pprAdd e1, text "+", pprAdd e2]
pprAdd other = pprMul other 

pprMul :: L0 -> Doc 
pprMul (LMul e1 e2) 
  = hsep [pprMul e1, text "*", pprMul e2]
pprMul other = pprFact other 

pprFact :: L0 -> Doc
pprFact (LVal v) = ppr v 
pprFact other = parens (ppr other)
