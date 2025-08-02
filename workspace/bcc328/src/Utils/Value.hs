module Utils.Value where

import Data.Char
import Utils.Pretty

data Value
  = VInt Int
  | VStr String
  | VBool Bool
  | VUnit
    deriving (Eq, Ord, Show)

instance Pretty Value where
  ppr (VInt n) = int n
  ppr (VStr s) = text s
  ppr (VBool b) = text $ if b then "true" else "false"
  ppr VUnit = text "unit"

(.+.) :: Value -> Value -> Either String Value
(VInt n1) .+. (VInt n2) = Right (VInt (n1 + n2))
e1 .+. e2 = Left $ unwords ["Type error on:", pretty e1, "+", pretty e2]

(.*.) :: Value -> Value -> Either String Value
(VInt n1) .*. (VInt n2) = Right (VInt (n1 * n2))
e1 .*. e2 = Left $ unwords ["Type error on:", pretty e1, "*", pretty e2]

(.-.) :: Value -> Value -> Either String Value
(VInt n1) .-. (VInt n2) = Right (VInt (n1 - n2))
e1 .-. e2 = Left $ unwords ["Type error on:", pretty e1, "-", pretty e2]

(./.) :: Value -> Value -> Either String Value
(VInt n1) ./. (VInt n2) = Right (VInt (n1 `div` n2))
e1 ./. e2 = Left $ unwords ["Type error on:", pretty e1, "/", pretty e2]

(.<.) :: Value -> Value -> Either String Value
(VInt n1) .<. (VInt n2) = Right (VBool (n1 < n2))
e1 .<. e2 = Left $ unwords ["Type error on:", pretty e1, "<", pretty e2]

(.=.) :: Value -> Value -> Either String Value
(VInt n1) .=. (VInt n2) = Right (VBool (n1 == n2))
(VBool b1) .=. (VBool b2) = Right (VBool (b1 == b2))
(VStr s1) .=. (VStr s2) = Right (VBool (s1 == s2))
e1 .=. e2 = Left $ unwords ["Type error on:", pretty e1, "=", pretty e2]

vand :: Value -> Value -> Either String Value
vand (VBool b1) (VBool b2) = Right (VBool (b1 && b2))
vand e1 e2 = Left $ unwords ["Type error on:", pretty e1, "&&", pretty e2]

vnot :: Value -> Either String Value
vnot (VBool b) = Right (VBool (not b))
vnot e = Left $ unwords ["Type error on: !", pretty e]

catValue :: Value -> Value -> Either String Value
catValue (VStr s1) (VStr s2) = Right (VStr (s1 ++ s2))
catValue e1 e2 = Left $ unwords ["Type error on: strcat(", pretty e1, ",", pretty e2, ")"]

size :: Value -> Either String Value
size (VStr s1) = Right (VInt (length s1))
size e1 = Left $ unwords ["Type error on: strsize(", pretty e1, ")"]

i2s :: Value -> Either String Value
i2s (VInt n1) = Right (VStr (show n1))
i2s e1 = Left $ unwords ["Type error on: i2s(", pretty e1, ")"]

i2b :: Value -> Either String Value
i2b (VInt n1) = Right (VBool (n1 /= 0))
i2b e1 = Left $ unwords ["Type error on: i2b(", pretty e1, ")"]

b2i :: Value -> Either String Value
b2i (VBool b) = Right $ if b then VInt 1 else VInt 0
b2i e1 = Left $ unwords ["Type error on: b2i(", pretty e1, ")"]

b2s :: Value -> Either String Value
b2s (VBool b) = Right $ if b then VStr "true" else VStr "false"
b2s e1 = Left $ unwords ["Type error on: b2s(", pretty e1, ")"]

s2i :: Value -> Either String Value
s2i e1@(VStr s)
  | all isDigit s && (not $ null s) = Right $ VInt (read s)
  | otherwise = Left $ unwords ["Type error on : s2i(", pretty e1, ")"]
s2i e1 = Left $ unwords ["Type error on : s2i(", pretty e1, ")"]

s2b :: Value -> Either String Value
s2b e1@(VStr s)
  | s `elem` ["true", "false"] = Right $ VBool (read s)
  | otherwise = Left $ unwords ["Type error on : s2b(", pretty e1, ")"]
s2b e1 = Left $ unwords ["Type error on : s2b(", pretty e1, ")"]


