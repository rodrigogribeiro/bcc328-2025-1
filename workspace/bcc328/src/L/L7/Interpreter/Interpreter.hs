module L.L7.Interpreter.Interpreter where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import L.L7.Frontend.Syntax

import System.IO

import Utils.Pretty
import Utils.Value
import Utils.Var


l7Interp :: Interpreter ()
l7Interp = do
    funs <- gets functions
    case Map.lookup (Var "main") funs of
      Just d7 -> do
          _ <- d7Interpreter d7 []
          pure ()
      Nothing -> throwError "Undefined main function!"

d7Interpreter :: D7 -> [Value] -> Interpreter Value
d7Interpreter (Fun v ps t ss) vs
  = do
      (val, _) <- withLocalEnv (zip ns vs) (blockInterpreter ss)
      pure val
    where
      ns = map (\ (Param n _) -> n) ps

blockInterpreter :: [S7] -> Interpreter (Value, [Var])
blockInterpreter [] = pure (VUnit, [])
blockInterpreter [s] = s7Interpreter s
blockInterpreter (s : ss)
  = do
      (_,vars1) <- s7Interpreter s
      (val, vars2) <- blockInterpreter ss
      removeFromEnv (vars1 ++ vars2)
      pure (val, [])

s7Interpreter :: S7 -> Interpreter (Value, [Var])
s7Interpreter (SLet v t e)
  = do
      val <- e7Interpreter e
      extEnv v val
      pure (VUnit, [v])
s7Interpreter (SAssign v e)
  = do
      val <- e7Interpreter e
      extEnv v val
      pure (VUnit, [])
s7Interpreter (SRead e v)
  = do
      (VStr s) <- e7Interpreter e
      runPrint s
      s1 <- VStr <$> liftIO getLine
      extEnv v s1
      pure (VUnit, [])
s7Interpreter (SPrint e)
  = do
      (VStr s) <- e7Interpreter e
      runPrint s
      pure (VUnit, [])
s7Interpreter (SIf e st se)
  = do
      (VBool b) <- e7Interpreter e
      if b then blockInterpreter st
      else blockInterpreter se
s7Interpreter s1@(SWhile e s)
  = do
      (VBool b) <- e7Interpreter e
      if b then do
        (val, _) <- blockInterpreter (s ++ [s1])
        pure (val, [])
      else pure (VUnit, [])
s7Interpreter (SReturn e)
  = do
      val <- e7Interpreter e
      pure (val, [])

e7Interpreter :: E7 -> Interpreter Value
e7Interpreter (EValue v) = pure v
e7Interpreter (EVar v _) = askMemory v
e7Interpreter (EAdd e1 e2) = binOpIntepreter e1 e2 (.+.)
e7Interpreter (EMult e1 e2) = binOpIntepreter e1 e2 (.*.)
e7Interpreter (EMinus e1 e2) = binOpIntepreter e1 e2 (.-.)
e7Interpreter (EDiv e1 e2) = binOpIntepreter e1 e2 (./.)
e7Interpreter (ELt e1 e2) = binOpIntepreter e1 e2 (.<.)
e7Interpreter (EEq e1 e2) = binOpIntepreter e1 e2 (.=.)
e7Interpreter (EAnd e1 e2) = binOpIntepreter e1 e2 vand
e7Interpreter (ECat e1 e2) = binOpIntepreter e1 e2 catValue
e7Interpreter (ENot e) = unOpIntepreter e vnot
e7Interpreter (ESize e) = unOpIntepreter e size
e7Interpreter (EI2S e) = unOpIntepreter e i2s
e7Interpreter (EI2B e) = unOpIntepreter e i2b
e7Interpreter (ES2I e) = unOpIntepreter e s2i
e7Interpreter (ES2B e) = unOpIntepreter e s2b
e7Interpreter (EB2I e) = unOpIntepreter e b2i
e7Interpreter (EB2S e) = unOpIntepreter e b2s
e7Interpreter (SCall v es)
  = do
      vs <- mapM e7Interpreter es
      d7 <- askFunction v
      d7Interpreter d7 vs


unOpIntepreter :: E7 ->
                  (Value -> Either String Value) ->
                  Interpreter Value
unOpIntepreter e f
  = do
      v <- e7Interpreter e
      case f v of
        Left err -> throwError err
        Right val -> pure val

binOpIntepreter :: E7 ->
                   E7 ->
                   (Value -> Value -> Either String Value) ->
                   Interpreter Value
binOpIntepreter e1 e2 f
  = do
      v1 <- e7Interpreter e1
      v2 <- e7Interpreter e2
      case f v1 v2 of
        Left err -> throwError err
        Right val -> pure val

-- interpreter state

data Env
  = Env {
      functions :: Map Var D7
    , memory :: Map.Map Var Value
  } deriving (Eq, Ord, Show)

initEnv :: L7 -> Env
initEnv (L7 ds) = Env funs Map.empty
  where
    funs = foldr (\ d@(Fun n _ _ _) ac -> Map.insert n d ac) Map.empty ds

askMemory :: Var -> Interpreter Value
askMemory v
  = do
      m <- gets memory
      case Map.lookup v m of
        Nothing -> throwError $ "Undefined var:" ++ pretty v
        Just val -> pure val

askFunction :: Var -> Interpreter D7
askFunction v
  = do
      fs <- gets functions
      case Map.lookup v fs of
        Nothing -> throwError $ "Undefined function:" ++ pretty v
        Just d -> pure d

withLocalEnv :: [(Var, Value)] -> Interpreter a -> Interpreter a
withLocalEnv vss m
  = do
      old <- get
      mapM_ (uncurry extEnv) vss
      r <- m
      put old
      pure r

removeFromEnv :: [Var] -> Interpreter ()
removeFromEnv vs
  = modify (\ env -> env{memory = rem (memory env)})
    where
      rem m = foldr (\ v ac -> Map.delete v ac) m vs

extEnv :: Var -> Value -> Interpreter ()
extEnv v val
  = modify (\ env -> env{memory = Map.insert v val (memory env)})

runPrint :: String -> Interpreter ()
runPrint s
  = do
      liftIO $ putStr s
      liftIO $ hFlush stdout

-- interpreter monad

type Interpreter a = ExceptT String (StateT Env IO) a

runInterpreter :: Env -> Interpreter a -> IO ()
runInterpreter env m
  = do
      (v, _) <- runStateT (runExceptT m) env
      case v of
        Left err -> putStrLn err
        Right _ -> pure ()

l7Interpreter :: L7 -> IO ()
l7Interpreter l7 =
  runInterpreter (initEnv l7) l7Interp
