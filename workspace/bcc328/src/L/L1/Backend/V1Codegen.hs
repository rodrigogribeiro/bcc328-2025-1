module L.L1.Backend.V1Codegen where

import L.L1.Frontend.Syntax
import V.V1.Instr
import Utils.Value

v1Codegen :: L1 -> Code
v1Codegen (L1 ss1)
  = (concatMap s1Codegen ss1) ++ [Halt]

s1Codegen :: S1 -> Code
s1Codegen (LRead s v)
  = [Push (VStr s), Input, Store v]
s1Codegen (LPrint e1)
  = e1Codegen e1 ++ [Print]
s1Codegen (LAssign v e1)
  = e1Codegen e1 ++ [Store v]

e1Codegen :: E1 -> Code
e1Codegen (LVal v) = [Push v]
e1Codegen (LVar v) = [Load v]
e1Codegen (LAdd l0 l1)
  = e1Codegen l0 ++ e1Codegen l1 ++ [Add]
e1Codegen (LMinus l0 l1)
  = e1Codegen l1 ++ e1Codegen l0 ++ [Sub]
e1Codegen (LMul l0 l1)
  = e1Codegen l0 ++ e1Codegen l1 ++ [Mul]
e1Codegen (LDiv l0 l1)
  = e1Codegen l1 ++ e1Codegen l0 ++ [Div]
