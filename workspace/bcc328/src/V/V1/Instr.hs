{-# LANGUAGE FlexibleInstances #-}
module V.V1.Instr where 

import Utils.Pretty
import Utils.Value 
import Utils.Var 

data Instr 
  = Push Value 
  | Add 
  | Mul
  | Input String 
  | Print
  | Load Var 
  | Store Var 
  | Halt 
  deriving (Eq, Ord, Show)

type Code = [Instr]

instance Pretty Code where 
  ppr = hcat . punctuate newline . map ppr  
    where 
      newline = char '\n'

instance Pretty Instr where 
  ppr (Push v) = hsep [ text "push"
                      , ppr v
                      , semi 
                      ]
  ppr Add = text "add;"
  ppr Mul = text "mul;"
  ppr (Input s) = hsep [ text "input"
                       , parens $ doubleQuotes $ text s
                       ]
  ppr Print = text "print;"
  ppr (Load v) = hsep [ text "load"
                      , parens (ppr v)
                      , text ";"
                      ]
  ppr (Store v) = hstep [ text "store"
                        , parens (ppr v)
                        , text ";"
                        ]
  ppr Halt = text "halt;"
