module L.L5.Backend.V3Codegen where

import L.L5.Frontend.Syntax
import Utils.Value
import Utils.Var
import V.V3.Instr

l5Codegen :: L5 -> [Instr]
l5Codegen (L5 ss)
  = concatMap s5Codegen ss ++ [Halt]

s5Codegen :: S5 -> [Instr]
s5Codegen (SLet v _ e)
  = e5Codegen e ++ [Store v]
s5Codegen (SAssign v e)
  = e5Codegen e ++ [Store v]
s5Codegen (SRead e v)
  = e5Codegen e ++ [Print, Input, Store v]
s5Codegen (SPrint e)
  = e5Codegen e ++ [Print]
s5Codegen (SWhile e bt)
  = concat [ce, [Not, JumpIf n], ct, [Jump d]]
    where
      ce = e5Codegen e
      m = length ce
      ct = concatMap s5Codegen bt
      n = length ct + 2
      d = - (n + m + 1)

s5Codegen (SIf e bt be)
  = concat [ e5Codegen e
           , [Not, JumpIf n]
           , ct
           , [Jump m]
           , ce
           ]
    where
      ct = concatMap s5Codegen bt
      ce = concatMap s5Codegen be
      n = length ct + 5
      m = length ce + 1

e5Codegen :: E5 -> [Instr]
e5Codegen (EValue v) = [Push v]
e5Codegen (EVar v _) = [Load v]
e5Codegen (EAdd e1 e2)
  = binopCodegen e1 e2 Add
e5Codegen (EMult e1 e2)
  = binopCodegen e1 e2 Mul
e5Codegen (EMinus e1 e2)
  = binopCodegen e1 e2 Sub
e5Codegen (EDiv e1 e2)
  = binopCodegen e1 e2 Div
e5Codegen (ELt e1 e2)
  = binopCodegen e1 e2 Lt
e5Codegen (EEq e1 e2)
  = binopCodegen e1 e2 IEq
e5Codegen (EAnd e1 e2)
  = binopCodegen e1 e2 And
e5Codegen (ENot e)
  = e5Codegen e ++ [Not]
e5Codegen (ECat e1 e2)
  = binopCodegen e1 e2 Cat
e5Codegen (ESize e)
  = e5Codegen e ++ [Size]
e5Codegen (EI2S e)
  = e5Codegen e ++ [I2S]
e5Codegen (EI2B e)
  = e5Codegen e ++ [I2B]
e5Codegen (EB2S e)
  = e5Codegen e ++ [B2S]
e5Codegen (EB2I e)
  = e5Codegen e ++ [B2I]
e5Codegen (ES2I e)
  = e5Codegen e ++ [S2I]
e5Codegen (ES2B e)
  = e5Codegen e ++ [S2B]

binopCodegen :: E5 -> E5 -> Instr -> [Instr]
binopCodegen e1 e2 i
  = concat [ e5Codegen e2
           , e5Codegen e1
           , [i]
           ]
