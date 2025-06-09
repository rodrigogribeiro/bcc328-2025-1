module L.L1.Frontend.Syntax where 

import Utils.Pretty 
import Utils.Value
import Utils.Var

-- definition of the syntax of L1 
-- programs
-- each L1 program is just a sequence of 
-- statements 
data L1 
  = L1 [S1] 
    deriving (Eq, Ord, Show)

-- statements can be a read, print or 
-- an assignment.

data S1
  = LRead String Var 
  | LPrint E1 
  | LAssign Var E1 
  deriving (Eq, Ord, Show)

-- expressions

data E1 
  = LVal Value 
  | LVar Var 
  | LAdd E1 E1
  | LMinus E1 E1
  | LMul E1 E1
  | LDiv E1 E1
  deriving (Eq, Ord, Show)

instance Pretty L1 where 
  ppr (L1 ss) 
    = hcat (punctuate eol (map ppr ss))
      where 
        eol = text ";\n"

instance Pretty S1 where 
  ppr (LRead s v) 
    = hsep [ text "read("
           , doubleQuotes (text s)
           , comma 
           , ppr v 
           , text ")"
           ]
  ppr (LPrint e) 
    = hsep [ text "print("
           , ppr e
           , text ")"
           ]
  ppr (LAssign v e)
    = hsep [ ppr v 
           , text "="
           , ppr e 
           ]

instance Pretty E1 where 
  ppr = pprAdd

pprAdd :: E1 -> Doc
pprAdd (LAdd e1 e2)
  = hsep [pprAdd e1, text "+", pprAdd e2]
pprAdd other = pprMul other 

pprMul :: E1 -> Doc 
pprMul (LMul e1 e2) 
  = hsep [pprMul e1, text "*", pprMul e2]
pprMul other = pprFact other 

pprFact :: E1 -> Doc
pprFact (LVal v) = ppr v
pprFact (LVar v) = ppr v 
pprFact other = parens (ppr other)
