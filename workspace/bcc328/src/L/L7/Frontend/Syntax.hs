module L.L7.Frontend.Syntax where

import Utils.Pretty
import Utils.Value
import Utils.Var

-- definition of the L7 syntax

data L7
  = L7 [D7]
    deriving (Eq, Ord, Show)

data Param = Param Var Ty
    deriving (Eq, Ord, Show)

data D7
  = Fun Var [Param] Ty [S7]
    deriving (Eq, Ord, Show)

data Ty
  = TyString
  | TyInt
  | TyBool
  | TyUnit
  | TyArrow Ty Ty
    deriving (Eq, Ord, Show)

data S7
  = SLet Var Ty E7
  | SAssign Var E7
  | SRead E7 Var
  | SPrint E7
  | SIf E7 [S7] [S7]
  | SWhile E7 [S7]
  | SReturn E7
  deriving (Eq, Ord, Show)

data E7
  = EValue Value
  -- initially, Nothing, after type checking
  -- we include its type.
  | EVar Var (Maybe Ty)
  -- arithmetic operators
  | EAdd E7 E7
  | EMult E7 E7
  | EMinus E7 E7
  | EDiv E7 E7
  -- Relational operators
  | ELt E7 E7
  | EEq E7 E7
  -- Boolean operators
  | EAnd E7 E7
  | ENot E7
  -- string operators
  | ECat E7 E7
  | ESize E7
  -- type coercion
  | EI2S E7
  | EI2B E7
  | ES2I E7
  | ES2B E7
  | EB2S E7
  | EB2I E7
  -- function call
  | SCall Var [E7]
  deriving (Eq, Ord, Show)

instance Pretty L7 where
  ppr (L7 ds)
    = vcat (map ppr ds)

instance Pretty Param where
  ppr (Param n t)
    = ppr n <+> text ":" <+> ppr t

instance Pretty D7 where
  ppr (Fun n ps t ss)
    = hsep [ text "fun"
           , ppr n
           , parens (hcat (punctuate comma (map ppr ps)))
           , text "->"
           , ppr t
           , text "do"]
      $$ (nest 3 (vcat (map ppr ss)))
      $$ text "end"

instance Pretty S7 where
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
        (nest 3 st') $$ text "end"
        where
        st' = vcat (map ppr st)
  ppr (SIf e st [])
    = text "if" <+> ppr e <+>
      text "then" $$ (nest 3 st')
    where
      st' = vcat (map ppr st)
  ppr (SIf e st se)
    = text "if" <+> ppr e <+>
      text "then" $$ (nest 3 st') $$
      text "else" $$ (nest 3 se')
    where
      st' = vcat (map ppr st)
      se' = vcat (map ppr se)
  ppr (SReturn e) = text "return" <+> ppr e <+> text ";"

instance Pretty Ty where
  ppr TyInt = text "int"
  ppr TyBool = text "bool"
  ppr TyString = text "string"
  ppr TyUnit = text "unit"
  ppr (TyArrow t1 t2)
    | isArrow t1 = parens (ppr t1) <+> text "->" <+> ppr t2
    | otherwise = ppr t1 <+> text "->" <+> ppr t2

isArrow :: Ty -> Bool
isArrow (TyArrow _ _) = True
isArrow _ = False

instance Pretty E7 where
  ppr = pprAnd

pprAnd :: E7 -> Doc
pprAnd (EAnd e1 e2)
  = hsep [pprAnd e1, text "&&", pprAnd e2]
pprAnd other = pprRel other

pprRel :: E7 -> Doc
pprRel (EEq e1 e2)
  = hsep [pprRel e1, text "=", pprRel e2]
pprRel (ELt e1 e2)
  = hsep [pprRel e1, text "<", pprRel e2]
pprRel other = pprAdd other

pprAdd :: E7 -> Doc
pprAdd (EAdd e1 e2)
  = hsep [pprAdd e1, text "+", pprAdd e2]
pprAdd (EMinus e1 e2)
  = hsep [pprAdd e1, text "-", pprAdd e2]
pprAdd other = pprMul other

pprMul :: E7 -> Doc
pprMul (EMult e1 e2)
  = hsep [pprMul e1, text "*", pprMul e2]
pprMul (EDiv e1 e2)
  = hsep [pprMul e1, text "/", pprMul e2]
pprMul other = pprFun other

pprFun :: E7 -> Doc
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
pprFun (SCall v es)
  = hsep [ppr v, text "(", es', text ")"]
    where
      es' = hcat (punctuate comma (map ppr es))
pprFun other = pprFact other

pprFact :: E7 -> Doc
pprFact (EValue (VStr s)) = text s
pprFact (EValue v) = ppr v
pprFact (EVar v _) = ppr v
pprFact other = parens (ppr other)
