module L.L6.Frontend.Syntax where

import Utils.Pretty
import Utils.Value
import Utils.Var

-- definition of the L6 syntax

data L6
  = L6 [S6]
    deriving (Eq, Ord, Show)

data Ty
  = TyString | TyInt | TyBool
    deriving (Eq, Ord, Show)

data S6
  = SLet Var Ty E6
  | SAssign Var E6
  | SRead E6 Var
  | SPrint E6
  -- if-then-else
  | SIf E6 [S6] [S6]
  | SWhile E6 [S6]
  deriving (Eq, Ord, Show)

data E6
  = EValue Value
  -- initially, Nothing, after type checking
  -- we include its type.
  | EVar Var (Maybe Ty)
  -- arithmetic operators
  | EAdd E6 E6
  | EMult E6 E6
  | EMinus E6 E6
  | EDiv E6 E6
  -- Relational operators
  | ELt E6 E6
  | EEq E6 E6
  -- Boolean operators
  | EAnd E6 E6
  | ENot E6
  -- string operators
  | ECat E6 E6
  | ESize E6
  -- type coercion
  | EI2S E6
  | EI2B E6
  | ES2I E6
  | ES2B E6
  | EB2S E6
  | EB2I E6
  deriving (Eq, Ord, Show)

instance Pretty L6 where
  ppr (L6 ss)
    = text "program" $$ (nest 3 (vcat $ map ppr ss)) $$ text "end"

instance Pretty S6 where
  ppr (SRead s v)
    = hsep [ text "read("
           , ppr s
           , comma
           , ppr v
           , text ");"
           ]
  ppr (SPrint e)
    = hsep [ text "print("
           , ppr e
           , text ");"
           ]
  ppr (SAssign v e)
    = hsep [ ppr v
           , text "="
           , ppr e
           , text ";"
           ]
  ppr (SLet v t e)
    = hsep [ text "let"
           , ppr v
           , text ":"
           , ppr t
           , text "="
           , ppr e
           , text ";"
           ]
  ppr (SWhile e st)
    = text "while" <+> ppr e <+> text "do" $$
        nest 3 (vcat (map ppr st))
  ppr (SIf e st [])
    = text "if" <+> ppr e <+>
      text "then" $$ (nest 3 st') $$ text "end"
    where
      st' = vcat (map ppr st)
  ppr (SIf e st se)
    = text "if" <+> ppr e <+>
      text "then" $$ (nest 3 st') $$
      text "else" $$ (nest 3 se') $$
      text "end"
    where
      st' = vcat (map ppr st)
      se' = vcat (map ppr se)


instance Pretty Ty where
  ppr TyInt = text "int"
  ppr TyBool = text "bool"
  ppr TyString = text "string"

instance Pretty E6 where
  ppr = pprAnd

pprAnd :: E6 -> Doc
pprAnd (EAnd e1 e2)
  = hsep [pprAnd e1, text "&&", pprAnd e2]
pprAnd other = pprRel other

pprRel :: E6 -> Doc
pprRel (EEq e1 e2)
  = hsep [pprRel e1, text "=", pprRel e2]
pprRel (ELt e1 e2)
  = hsep [pprRel e1, text "<", pprRel e2]
pprRel other = pprAdd other

pprAdd :: E6 -> Doc
pprAdd (EAdd e1 e2)
  = hsep [pprAdd e1, text "+", pprAdd e2]
pprAdd (EMinus e1 e2)
  = hsep [pprAdd e1, text "-", pprAdd e2]
pprAdd other = pprMul other

pprMul :: E6 -> Doc
pprMul (EMult e1 e2)
  = hsep [pprMul e1, text "*", pprMul e2]
pprMul (EDiv e1 e2)
  = hsep [pprMul e1, text "/", pprMul e2]
pprMul other = pprFun other

pprFun :: E6 -> Doc
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

pprFact :: E6 -> Doc
pprFact (EValue v) = ppr v
pprFact (EVar v _) = ppr v
pprFact other = parens (ppr other)
