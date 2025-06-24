module L.L2.Frontend.Syntax where

import Utils.Value
import Utils.Var

-- definition of the syntax of L2
-- programs
-- each L2 program is just a sequence of
-- statements

data L2
  = L2 [S2]
    deriving (Eq, Ord, Show)

-- statements can be a read, print or
-- an assignment.

data S2
  = Def Var E2 [S2]
  | LRead String Var
  | LPrint E2
  | LAssign Var E2
  deriving (Eq, Ord, Show)

-- expressions

data E2
  = LVal Value
  | LVar Var
  | LAdd E2 E2
  | LMinus E2 E2
  | LMul E2 E2
  | LDiv E2 E2
  deriving (Eq, Ord, Show)


