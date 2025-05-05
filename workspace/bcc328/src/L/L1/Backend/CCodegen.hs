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
generateBody e 
  = [ generateAssignment e 
    , generatePrint 
    ]


generateAssignment :: L1 -> String 
generateAssignment e 
  = unwords [ "int val ="
            , pretty e 
            , ";" 
            ]

generatePrint :: String 
generatePrint = "printf(\"%d\", val);"
