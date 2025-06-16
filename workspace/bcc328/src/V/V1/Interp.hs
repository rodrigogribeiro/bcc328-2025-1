module V.V1.Interp where 

import Data.Map(Map)
import qualified Data.Map as Map

import V.V1.Instr 
import Utils.Pretty
import Utils.Value 
import Utils.Var 

type Stack = [Value]

data Conf 
  = Conf {
      stack :: Stack 
    , memory :: Map Var Value 
    } deriving (Eq, Ord, Show)

type Interp a = Conf -> IO (Either String Conf)

interp :: Code -> Interp () 
interp = error "Not implemented yet!"
