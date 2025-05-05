{-# LANGUAGE FlexibleInstances #-}
module V.V0.Instr where 

import Utils.Pretty 
import Utils.Value 

data Instr 
  = Push Value 
  | Add 
  | Mul 
  | Print
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
  ppr Print = text "print;"
  ppr Halt = text "halt;"
