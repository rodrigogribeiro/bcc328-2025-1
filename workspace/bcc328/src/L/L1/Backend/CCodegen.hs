module L.L1.Backend.CCodegen where

import L.L1.Frontend.Syntax
import Utils.Pretty

-- top level code generation function

cL1Codegen :: L1 -> String
cL1Codegen e
  = unlines $ [ "#include <stdio.h>"
              , "// code generated for expressions"
              , "int main () {" ] ++
              (map (nest 3) (generateBody e)) ++
              [ nest 3 $ "putchar('\\n');"
              , nest 3 "return 0;"
              , "}"
              ]
    where
      nest n v = replicate n ' ' ++ v

generateBody :: L1 -> [String]
generateBody (L1 ss)
  = map generateStmt ss

generateStmt :: S1 -> String
generateStmt (LAssign v e1)
  = unwords ["int", pretty v, "=", generateExp e1, ";"]
generateStmt (LPrint e1)
  = unwords ["printf(%d,", generateExp e1, ");"]
generateStmt (LRead s v)
  = unwords ["print(\"",s,"\");\n", "scanf(%d, &", pretty v, ")"]

generateExp :: E1 -> String
generateExp = pretty
