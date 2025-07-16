module V.V2.Interp where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.Bifunctor (bimap)
import Data.Char
import Data.Map(Map)
import qualified Data.Map as Map

import V.V2.Instr
import Utils.Pretty
import Utils.Value
import Utils.Var

interp :: Code -> IO (Either String Conf)
interp code = f <$> runStateT (runExceptT (exec code)) initConf
  where
    f (Left err, _) = Left err
    f (_, conf) = Right conf

exec :: Code -> Interp ()
exec = mapM_ execInstr

execInstr :: Instr -> Interp ()
execInstr (Push v) = push v
execInstr Add = binop (.+.)
execInstr Mul = binop (.*.)
execInstr Sub = binop (.-.)
execInstr Div = binop (./.)
execInstr Lt = binop (.<.)
execInstr IEq = binop (.=.)
execInstr And = binop vand
execInstr Not
  = do
      v <- pop
      case vnot v of
        Left err -> throwError err
        Right v' -> push v'
execInstr Input = input
execInstr Print = printValue
execInstr (Load v) = load v
execInstr (Store v) = store v
execInstr Halt = pure ()

type Stack = [Value]

data Conf
  = Conf {
      stack :: Stack
    , memory :: Map Var Value
    } deriving (Eq, Ord, Show)

initConf :: Conf
initConf = Conf [] Map.empty

type Interp a = ExceptT String (StateT Conf IO) a

push :: Value -> Interp ()
push v = modify (\ c -> c {stack = v : stack c})

pop :: Interp Value
pop = do
        stk <- gets stack
        case stk of
          (v : stk') -> do
            modify (\ c -> c{stack = stk'})
            pure v
          _ -> emptyStackError

binop :: (Value -> Value -> Either String Value) -> Interp ()
binop f = do
    v1 <- pop
    v2 <- pop
    case f v1 v2 of
      Left err -> throwError err
      Right v3 -> push v3

input :: Interp ()
input
  = do
      v <- pop
      case v of
        (VStr msg) -> do
            liftIO $ putStr msg
            input <- liftIO getLine
            unless (all isDigit input) (invalidInput input)
            push (VInt (read input))
        _ -> do
          invalidInput (pretty v)


printValue :: Interp ()
printValue = do
  v <- pop
  liftIO $ putStrLn $ pretty v

load :: Var -> Interp ()
load v = do
  m <- gets memory
  case Map.lookup v m of
    Just v -> push v
    Nothing -> uninitializedVariable v

store :: Var -> Interp ()
store v = do
  val <- pop
  modify (\ c -> c{memory = Map.insert v val (memory c)})

uninitializedVariable :: Var -> Interp a
uninitializedVariable v = throwError $ "Uninitialized variable:" ++ pretty v

emptyStackError :: Interp a
emptyStackError = throwError "Empty stack found during execution!"

invalidInput :: String -> Interp a
invalidInput s
  = throwError ("Invalid input:" ++ s)
