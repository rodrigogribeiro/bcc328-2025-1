module L.L5.Frontend.Syntax where

import Utils.Pretty
import Utils.Value
import Utils.Var

-- definition of the L5 syntax

data L5
  = L5 [S5]
    deriving (Eq, Ord, Show)

data Ty
  = TyString | TyInt | TyBool
    deriving (Eq, Ord, Show)

data S5
  = SLet Var Ty E5
  | SAssign Var E5
  | SRead E5 Var
  | SPrint E5
  -- if-then-else
  | SIf E5 [S5] [S5]
  | SWhile E5 [S5]
  deriving (Eq, Ord, Show)

data E5
  = EValue Value
  -- initially, Nothing, after type checking
  -- we include its type.
  | EVar Var (Maybe Ty)
  -- arithmetic operators
  | EAdd E5 E5
  | EMult E5 E5
  | EMinus E5 E5
  | EDiv E5 E5
  -- Relational operators
  | ELt E5 E5
  | EEq E5 E5
  -- Boolean operators
  | EAnd E5 E5
  | ENot E5
  -- string operators
  | ECat E5 E5
  | ESize E5
  -- type coercion
  | EI2S E5
  | EI2B E5
  | ES2I E5
  | ES2B E5
  | EB2S E5
  | EB2I E5
  deriving (Eq, Ord, Show)

instance Pretty L5 where
  ppr (L5 ss)
    = text "program" $$ (nest 3 (vcat $ map ppr ss)) $$ text "end"

instance Pretty S5 where
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

instance Pretty E5 where
  ppr = pprAnd

pprAnd :: E5 -> Doc
pprAnd (EAnd e1 e2)
  = hsep [pprAnd e1, text "&&", pprAnd e2]
pprAnd other = pprRel other

pprRel :: E5 -> Doc
pprRel (EEq e1 e2)
  = hsep [pprRel e1, text "=", pprRel e2]
pprRel (ELt e1 e2)
  = hsep [pprRel e1, text "<", pprRel e2]
pprRel other = pprAdd other

pprAdd :: E5 -> Doc
pprAdd (EAdd e1 e2)
  = hsep [pprAdd e1, text "+", pprAdd e2]
pprAdd (EMinus e1 e2)
  = hsep [pprAdd e1, text "-", pprAdd e2]
pprAdd other = pprMul other

pprMul :: E5 -> Doc
pprMul (EMult e1 e2)
  = hsep [pprMul e1, text "*", pprMul e2]
pprMul (EDiv e1 e2)
  = hsep [pprMul e1, text "/", pprMul e2]
pprMul other = pprFun other

pprFun :: E5 -> Doc
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

pprFact :: E5 -> Doc
pprFact (EValue v) = ppr v
pprFact (EVar v _) = ppr v
pprFact other = parens (ppr other)
