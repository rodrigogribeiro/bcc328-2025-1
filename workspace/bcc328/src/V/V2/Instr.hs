{-# LANGUAGE FlexibleInstances #-}
module V.V2.Instr where

import Utils.Pretty
import Utils.Value
import Utils.Var

data Instr
  = Push Value
  | Add
  | Sub
  | Mul
  | Div
  | Lt
  | IEq
  | And
  | Not
  | Cat
  | Size
  | I2S
  | I2B
  | B2I
  | B2S
  | S2I
  | S2B
  | Input
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
  ppr Sub = text "minus;"
  ppr Div = text "div;"
  ppr Lt = text "lt;"
  ppr IEq = text "eq;"
  ppr And = text "and;"
  ppr Not = text "not;"
  ppr Cat = text "cat;"
  ppr Size = text "size;"
  ppr I2S = text "i2s;"
  ppr I2B = text "i2b;"
  ppr B2I = text "b2i;"
  ppr B2S = text "b2s;"
  ppr S2I = text "s2i;"
  ppr S2B = text "s2b;"
  ppr Input = text "input;"
  ppr Print = text "print;"
  ppr (Load v) = hsep [ text "load"
                      , parens (ppr v)
                      , text ";"
                      ]
  ppr (Store v) = hsep [ text "store"
                       , parens (ppr v)
                       , text ";"
                       ]
  ppr Halt = text "halt;"
