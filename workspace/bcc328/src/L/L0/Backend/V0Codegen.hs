module L.L0.Backend.V0Codegen where

import L.L0.Frontend.Syntax
import V.V0.Instr 

v0Codegen :: L0 -> Code 
v0Codegen e = codegen' e ++ [Print, Halt]

codegen' :: L0 -> Code 
codegen' (LVal v) = [Push v]
codegen' (LAdd l0 l1) 
  = codegen' l0 ++ codegen' l1 ++ [Add]
codegen' (LMul l0 l1) 
  = codegen' l0 ++ codegen' l1 ++ [Mul]
