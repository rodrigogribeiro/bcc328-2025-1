{-# LANGUAGE FlexibleInstances #-}
module V.V4.Instr where

import Utils.Pretty
import Utils.Value
import Utils.Var

type Offset = Int

type Code = [Fun]

data Fun
  = Fun {
      name :: Var
    , instructions :: [Instr]
    } deriving (Eq, Ord, Show)

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
  | JumpIf Offset
  | Jump Offset
  | Call Var
  | Return
  | Halt
  deriving (Eq, Ord, Show)

instance Pretty a => Pretty [a] where
  ppr = hcat . punctuate newline . map ppr
    where
      newline = char '\n'

instance Pretty Fun where
  ppr (Fun v instrs)
    = text "fun" <+> ppr v <+>
        lbrace     $$
        ppr instrs $$
        rbrace

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
  ppr (JumpIf d) = text "jumpif" <+> int d <+> text ";"
  ppr (Jump d) = text "jump" <+> int d <+> text ";"
  ppr (Call v) = text "call" <+> ppr v <+> text ";"
  ppr Return = text "return ;"
  ppr Halt = text "halt;"
