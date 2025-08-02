module L.L4.Backend.V3Codegen where

import L.L4.Frontend.Syntax
import Utils.Value
import Utils.Var
import V.V3.Instr

l4Codegen :: L4 -> [Instr]
l4Codegen (L4 ss)
  = concatMap s4Codegen ss ++ [Halt]

s4Codegen :: S4 -> [Instr]
s4Codegen (SLet v _ e)
  = e4Codegen e ++ [Store v]
s4Codegen (SAssign v e)
  = e4Codegen e ++ [Store v]
s4Codegen (SRead e v)
  = e4Codegen e ++ [Print, Input, Store v]
s4Codegen (SPrint e)
  = e4Codegen e ++ [Print]
s4Codegen (SIf e bt be)
  = concat [ e4Codegen e
           , [Not, JumpIf n]
           , ct
           , [Jump m]
           , ce
           ]
    where
      ct = concatMap s4Codegen bt
      ce = concatMap s4Codegen be
      n = length ct + 4
      m = length ce + 1

e4Codegen :: E4 -> [Instr]
e4Codegen (EValue v) = [Push v]
e4Codegen (EVar v _) = [Load v]
e4Codegen (EAdd e1 e2)
  = binopCodegen e1 e2 Add
e4Codegen (EMult e1 e2)
  = binopCodegen e1 e2 Mul
e4Codegen (EMinus e1 e2)
  = binopCodegen e1 e2 Sub
e4Codegen (EDiv e1 e2)
  = binopCodegen e1 e2 Div
e4Codegen (ELt e1 e2)
  = binopCodegen e1 e2 Lt
e4Codegen (EEq e1 e2)
  = binopCodegen e1 e2 IEq
e4Codegen (EAnd e1 e2)
  = binopCodegen e1 e2 And
e4Codegen (ENot e)
  = e4Codegen e ++ [Not]
e4Codegen (ECat e1 e2)
  = binopCodegen e1 e2 Cat
e4Codegen (ESize e)
  = e4Codegen e ++ [Size]
e4Codegen (EI2S e)
  = e4Codegen e ++ [I2S]
e4Codegen (EI2B e)
  = e4Codegen e ++ [I2B]
e4Codegen (EB2S e)
  = e4Codegen e ++ [B2S]
e4Codegen (EB2I e)
  = e4Codegen e ++ [B2I]
e4Codegen (ES2I e)
  = e4Codegen e ++ [S2I]
e4Codegen (ES2B e)
  = e4Codegen e ++ [S2B]

binopCodegen :: E4 -> E4 -> Instr -> [Instr]
binopCodegen e1 e2 i
  = concat [ e4Codegen e2
           , e4Codegen e1
           , [i]
           ]
