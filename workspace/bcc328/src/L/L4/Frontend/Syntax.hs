
module L.L4.Frontend.Syntax where

import Utils.Pretty
import Utils.Value
import Utils.Var

-- definition of the L4 syntax

data L4
  = L4 [S4]
    deriving (Eq, Ord, Show)

data Ty
  = TyString | TyInt | TyBool
    deriving (Eq, Ord, Show)

data S4
  = SLet Var Ty E4
  | SAssign Var E4
  | SRead E4 Var
  | SPrint E4
  -- if-then-else
  | SIf E4 [S4] [S4]
  deriving (Eq, Ord, Show)

data E4
  = EValue Value
  -- initially, Nothing, after type checking
  -- we include its type.
  | EVar Var (Maybe Ty)
  -- arithmetic operators
  | EAdd E4 E4
  | EMult E4 E4
  | EMinus E4 E4
  | EDiv E4 E4
  -- Relational operators
  | ELt E4 E4
  | EEq E4 E4
  -- Boolean operators
  | EAnd E4 E4
  | ENot E4
  -- string operators
  | ECat E4 E4
  | ESize E4
  -- type coercion
  | EI2S E4
  | EI2B E4
  | ES2I E4
  | ES2B E4
  | EB2S E4
  | EB2I E4
  deriving (Eq, Ord, Show)

instance Pretty L4 where
  ppr (L4 ss)
    = hcat (punctuate eol (map ppr ss))
      where
        eol = text ";\n"

instance Pretty S4 where
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
  ppr (SIf e st se)
    = text "if" <+> ppr e <+>
      text "then" $$ (nest 3 st') $$
      text "else" $$ (nest 4 se')
    where
      st' = hcat $ punctuate eol (map ppr st)
      se' = hcat $ punctuate eol (map ppr se)
      eol = text "\n"


instance Pretty Ty where
  ppr TyInt = text "int"
  ppr TyBool = text "bool"
  ppr TyString = text "string"

instance Pretty E4 where
  ppr = pprAnd

pprAnd :: E4 -> Doc
pprAnd (EAnd e1 e2)
  = hsep [pprAnd e1, text "&&", pprAnd e2]
pprAnd other = pprRel other

pprRel :: E4 -> Doc
pprRel (EEq e1 e2)
  = hsep [pprRel e1, text "=", pprRel e2]
pprRel (ELt e1 e2)
  = hsep [pprRel e1, text "<", pprRel e2]
pprRel other = pprAdd other

pprAdd :: E4 -> Doc
pprAdd (EAdd e1 e2)
  = hsep [pprAdd e1, text "+", pprAdd e2]
pprAdd (EMinus e1 e2)
  = hsep [pprAdd e1, text "-", pprAdd e2]
pprAdd other = pprMul other

pprMul :: E4 -> Doc
pprMul (EMult e1 e2)
  = hsep [pprMul e1, text "*", pprMul e2]
pprMul (EDiv e1 e2)
  = hsep [pprMul e1, text "/", pprMul e2]
pprMul other = pprFun other

pprFun :: E4 -> Doc
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

pprFact :: E4 -> Doc
pprFact (EValue v) = ppr v
pprFact (EVar v _) = ppr v
pprFact other = parens (ppr other)
