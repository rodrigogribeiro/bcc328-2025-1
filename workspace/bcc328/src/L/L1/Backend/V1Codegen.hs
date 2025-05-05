module L.L1.Backend.V0Codegen where

import L.L1.Frontend.Syntax
import V.V1.Instr 

v1Codegen :: L1 -> Code 
v1Codegen e = codegen' e ++ [Print, Halt]

codegen' :: L1 -> Code 
codegen' (LVal v) = [Push v]
codegen' (LAdd l0 l1) 
  = codegen' l0 ++ codegen' l1 ++ [Add]
codegen' (LMul l0 l1) 
  = codegen' l0 ++ codegen' l1 ++ [Mul]
