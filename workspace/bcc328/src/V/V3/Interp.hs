module V.V3.Interp where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.Bifunctor (bimap)
import Data.Char
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map

import V.V3.Instr
import Utils.Pretty
import Utils.Value
import Utils.Var

interp :: Code -> IO (Either String Conf)
interp code = f <$> runStateT (runExceptT exec) (initConf code)
  where
    f (Left err, _) = Left err
    f (_, conf) = Right conf

exec :: Interp ()
exec = do
          i <- fetchInstr
          if i == Halt then pure ()
            else do
              execInstr i
              exec

fetchInstr :: Interp Instr
fetchInstr = do
  idx <- gets pc
  cs <- gets code
  case cs !? idx of
    Just i -> pure i
    Nothing -> throwError "Index out of bounds for PC"

execInstr :: Instr -> Interp ()
execInstr (Push v) = push v
execInstr Add = binop (.+.)
execInstr Mul = binop (.*.)
execInstr Sub = binop (.-.)
execInstr Div = binop (./.)
execInstr Lt = binop (.<.)
execInstr IEq = binop (.=.)
execInstr And = binop vand
execInstr Not = do
  v <- pop
  case vnot v of
    Left err -> throwError err
    Right v' -> push v'
execInstr Cat =  binop catValue
execInstr Size = do
  v <- pop
  case size v of
    Left err -> throwError err
    Right v' -> push v'
execInstr I2S = do
  v <- pop
  case i2s v of
    Left err -> throwError err
    Right v' -> push v'
execInstr I2B = do
  v <- pop
  case i2b v of
    Left err -> throwError err
    Right v' -> push v'
execInstr S2I = do
  v <- pop
  case s2i v of
    Left err -> throwError err
    Right v' -> push v'
execInstr S2B = do
  v <- pop
  case s2b v of
    Left err -> throwError err
    Right v' -> push v'
execInstr B2S = do
  v <- pop
  case b2s v of
    Left err -> throwError err
    Right v' -> push v'
execInstr B2I = do
  v <- pop
  case b2i v of
    Left err -> throwError err
    Right v' -> push v'
execInstr Input = input
execInstr Print = printValue
execInstr (Load v) = load v
execInstr (Store v) = store v
execInstr (JumpIf d) = do
  v <- pop
  case v of
    VBool b -> if b then modifyPC d else modifyPC 1
    _ -> throwError "Type error! Expected boolean!"
execInstr (Jump d) = modifyPC d
execInstr Halt = pure ()

type Stack = [Value]

data Conf
  = Conf {
      pc :: Int
    , code :: Code
    , stack :: Stack
    , memory :: Map Var Value
    } deriving (Eq, Ord, Show)

initConf :: Code -> Conf
initConf cs = Conf 0 cs [] Map.empty

type Interp a = ExceptT String (StateT Conf IO) a

modifyPC :: Offset -> Interp ()
modifyPC off
  = modify (\ c -> c{pc = off + (pc c)})

push :: Value -> Interp ()
push v
  = do
    modify (\ c -> c {stack = v : stack c})
    modifyPC 1

pop :: Interp Value
pop = do
        stk <- gets stack
        case stk of
          (v : stk') -> do
            modify (\ c -> c{stack = stk'})
            modifyPC 1
            pure v
          _ -> emptyStackError

binop :: (Value -> Value -> Either String Value) -> Interp ()
binop f = do
    v1 <- pop
    v2 <- pop
    case f v1 v2 of
      Left err -> throwError err
      Right v3 -> do
        push v3
        modifyPC 1

input :: Interp ()
input
  = do
      v <- pop
      case v of
        (VStr msg) -> do
            liftIO $ putStr msg
            input <- liftIO getLine
            push (VStr input)
            modifyPC 1
        _ -> do
          invalidInput (pretty v)

printValue :: Interp ()
printValue = do
  v <- pop
  liftIO $ putStrLn $ pretty v
  modifyPC 1

load :: Var -> Interp ()
load v = do
  m <- gets memory
  case Map.lookup v m of
    Just v -> do
      push v
      modifyPC 1
    Nothing -> uninitializedVariable v

store :: Var -> Interp ()
store v = do
  val <- pop
  modify (\ c -> c{memory = Map.insert v val (memory c)})
  modifyPC 1

uninitializedVariable :: Var -> Interp a
uninitializedVariable v = throwError $ "Uninitialized variable:" ++ pretty v

emptyStackError :: Interp a
emptyStackError = throwError "Empty stack found during execution!"

invalidInput :: String -> Interp a
invalidInput s
  = throwError ("Invalid input:" ++ s)
