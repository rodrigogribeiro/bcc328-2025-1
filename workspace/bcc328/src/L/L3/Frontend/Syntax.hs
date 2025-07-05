module L.L3.Frontend.Syntax where

import Utils.Pretty
import Utils.Value
import Utils.Var

-- definition of the L3 syntax

data L3
  = L3 [S3]
    deriving (Eq, Ord, Show)

data Ty
  = TString | TInt | TBool
    deriving (Eq, Ord, Show)

data S3
  = SLet Var Ty E3
  | SAssign Var E3
  | SRead E3 Var
  | SPrint E3
  deriving (Eq, Ord, Show)

data E3
  = EValue Value
  -- initially, Nothing, after type checking
  -- we include its type.
  | EVar Var (Maybe Ty)
  -- arithmetic operators
  | EAdd E3 E3
  | EMult E3 E3
  | EMinus E3 E3
  | EDiv E3 E3
  -- Relational operators
  | ELt E3 E3
  | EEq E3 E3
  -- Boolean operators
  | EAnd E3 E3
  | ENot E3
  -- string operators
  | ECat E3 E3
  | ESize E3
  -- type coercion
  | EI2S E3
  | EI2B E3
  | ES2I E3
  | ES2B E3
  | EB2S E3
  | EB2I E3
  deriving (Eq, Ord, Show)

instance Pretty L3 where
  ppr (L3 ss)
    = hcat (punctuate eol (map ppr ss))
      where
        eol = text ";\n"

instance Pretty S3 where
  ppr (SRead s v)
    = hsep [ text "read("
           , ppr s
           , comma
           , ppr v
           , text ")"
           ]
  ppr (SPrint e)
    = hsep [ text "print("
           , ppr e
           , text ")"
           ]
  ppr (SAssign v e)
    = hsep [ ppr v
           , text "="
           , ppr e
           ]
  ppr (SLet v t e)
    = hsep [ text "let"
           , ppr v
           , text ":"
           , ppr t
           , text "="
           , ppr e
           ]

instance Pretty Ty where
  ppr TInt = text "int"
  ppr TBool = text "bool"
  ppr TString = text "string"

instance Pretty E3 where
  ppr = pprAnd

pprAnd :: E3 -> Doc
pprAnd (EAnd e1 e2)
  = hsep [pprAnd e1, text "&&", pprAnd e2]
pprAnd other = pprRel other

pprRel :: E3 -> Doc
pprRel (EEq e1 e2)
  = hsep [pprRel e1, text "=", pprRel e2]
pprRel (ELt e1 e2)
  = hsep [pprRel e1, text "<", pprRel e2]
pprRel other = pprAdd other

pprAdd :: E3 -> Doc
pprAdd (EAdd e1 e2)
  = hsep [pprAdd e1, text "+", pprAdd e2]
pprAdd (EMinus e1 e2)
  = hsep [pprAdd e1, text "-", pprAdd e2]
pprAdd other = pprMul other

pprMul :: E3 -> Doc
pprMul (EMult e1 e2)
  = hsep [pprMul e1, text "*", pprMul e2]
pprMul (EDiv e1 e2)
  = hsep [pprMul e1, text "/", pprMul e2]
pprMul other = pprFun other

pprFun :: E3 -> Doc
pprFun (ENot e) = hsep [text "!", pprFun e]
pprFun (ECat e1 e2)
  = hsep [text "strcat", text "(", ppr e1, comma, ppr e2, text ")"]
pprFun (ESize e1)
  = hsep [text "strsize", text "(", ppr e1, text ")"]
pprFun (EI2S e1)
  = hsep [text "i2s", text "(", ppr e1, text ")"]
pprFun (EI2B e1)
  = hsep [text "i2b", text "(", ppr e1, text ")"]
pprFun (ES2I e1)
  = hsep [text "s2i", text "(", ppr e1, text ")"]
pprFun (ES2B e1)
  = hsep [text "s2b", text "(", ppr e1, text ")"]
pprFun (EB2S e1)
  = hsep [text "b2s", text "(", ppr e1, text ")"]
pprFun (EB2I e1)
  = hsep [text "b2i", text "(", ppr e1, text ")"]
pprFun other = pprFact other

pprFact :: E3 -> Doc
pprFact (EValue v) = ppr v
pprFact (EVar v _) = ppr v
pprFact other = parens (ppr other)
