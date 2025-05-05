module L.L0.Backend.CCodegen where

import L.L0.Frontend.Syntax 
import Utils.Pretty 

-- top level code generation function 

cL0Codegen :: L0 -> String 
cL0Codegen e 
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

generateBody :: L0 -> [String] 
generateBody e 
  = [ generateAssignment e 
    , generatePrint 
    ]


generateAssignment :: L0 -> String 
generateAssignment e 
  = unwords [ "int val ="
            , pretty e 
            , ";" 
            ]

generatePrint :: String 
generatePrint = "printf(\"%d\", val);"
