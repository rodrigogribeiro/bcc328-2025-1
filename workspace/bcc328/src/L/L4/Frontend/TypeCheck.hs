module L.L4.Frontend.TypeCheck where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.Map (Map)
import qualified Data.Map as Map

import L.L4.Frontend.Syntax

import Utils.Pretty
import Utils.Value
import Utils.Var

-- type checking algorithm

typeCheck :: L4 -> Either String L4
typeCheck l4 = fst $ fst $ runTcM initTcEnv (tcL4 l4)

-- typing L4 programs

tcL4 :: L4 -> TcM L4
tcL4 (L4 ss) = L4 <$> mapM tcS4 ss

-- typing L4 statements

tcS4 :: S4 -> TcM S4
tcS4 (SLet v t e)
  = do
      v_defined <- isDefinedVar v
      when v_defined (undefinedVar v)
      (e', t') <- tcE4 e
      unless (t == t') (incompatibleTypes t t')
      addDecl v t
      pure (SLet v t e')
tcS4 (SAssign v e)
  = do
      t <- askEnv v
      (e', t') <- tcE4 e
      unless (t == t') (incompatibleTypes t t')
      pure (SAssign v e')
tcS4 (SRead e v)
  = do
      t <- askEnv v
      (e', t') <- tcE4 e
      unless (t' == TyString) (incompatibleTypes TyString t')
      pure (SRead e' v)
tcS4 (SPrint e)
  = do
      (e', t) <- tcE4 e
      unless (t == TyString) (incompatibleTypes TyString t)
      pure (SPrint e)
tcS4 (SIf e st se)
  = do
      (e', t) <- tcE4 e
      unless (t == TyBool) (incompatibleTypes TyBool t)
      st' <- withLocalEnv (mapM tcS4 st)
      se' <- withLocalEnv (mapM tcS4 se)
      pure (SIf e' st' se')

-- typing L4 expressions

tcE4 :: E4 -> TcM (E4, Ty)
tcE4 (EValue v)
  = do
      t <- tcValue v
      pure (EValue v, t)
tcE4 (EVar v Nothing)
  = do
      t <- askEnv v
      pure (EVar v (Just t), t)
tcE4 (EAdd e1 e2)
  = tcBinOp EAdd (e1, TyInt) (e2, TyInt) TyInt
tcE4 (EMinus e1 e2)
  = tcBinOp EMinus (e1, TyInt) (e2, TyInt) TyInt
tcE4 (EMult e1 e2)
  = tcBinOp EMult (e1, TyInt) (e2, TyInt) TyInt
tcE4 (EDiv e1 e2)
  = tcBinOp EDiv (e1, TyInt) (e2, TyInt) TyInt
tcE4 (EAnd e1 e2)
  = tcBinOp EAnd (e1, TyBool) (e2, TyBool) TyBool
tcE4 (ECat e1 e2)
  = tcBinOp ECat (e1, TyString) (e2, TyString) TyString
tcE4 (ENot e1)
  = do
      (e', t) <- tcE4 e1
      unless (t == TyBool)(incompatibleTypes t TyBool)
      pure (ENot e', TyBool)
tcE4 (ESize e1)
  = do
      (e', t) <- tcE4 e1
      unless (t == TyString)(incompatibleTypes t TyString)
      pure (ESize e', TyInt)
tcE4 (ELt e1 e2)
  = do
      (e1', t1) <- tcE4 e1
      (e2', t2) <- tcE4 e2
      unless (t1 == t2) (incompatibleTypes t1 t2)
      pure (ELt e1' e2', TyBool)
tcE4 (EEq e1 e2)
  = do
      (e1', t1) <- tcE4 e1
      (e2', t2) <- tcE4 e2
      unless (t1 == t2) (incompatibleTypes t1 t2)
      pure (EEq e1' e2', TyBool)
tcE4 (EI2S e1)
  = do
      (e1', t) <- tcE4 e1
      unless (t == TyInt) (incompatibleTypes t TyInt)
      pure (EI2S e1', TyString)
tcE4 (EI2B e1)
  = do
      (e1', t) <- tcE4 e1
      unless (t == TyInt) (incompatibleTypes t TyInt)
      pure (EI2S e1', TyBool)
tcE4 (ES2I e1)
  = do
      (e1', t) <- tcE4 e1
      unless (t == TyString) (incompatibleTypes t TyString)
      pure (EI2S e1', TyInt)
tcE4 (ES2B e1)
  = do
      (e1', t) <- tcE4 e1
      unless (t == TyString) (incompatibleTypes t TyString)
      pure (ES2B e1', TyBool)
tcE4 (EB2S e1)
  = do
      (e1', t) <- tcE4 e1
      unless (t == TyBool) (incompatibleTypes t TyBool)
      pure (EB2S e1', TyString)
tcE4 (EB2I e1)
  = do
      (e1', t) <- tcE4 e1
      unless (t == TyBool) (incompatibleTypes t TyBool)
      pure (EI2S e1', TyInt)

-- typing L4 values

tcValue :: Value -> TcM Ty
tcValue (VInt _) = pure TyInt
tcValue (VStr _) = pure TyString
tcValue (VBool _) = pure TyBool

-- typing binary operators

tcBinOp :: (E4 -> E4 -> E4) ->
           (E4, Ty) ->
           (E4, Ty) ->
           Ty -> TcM (E4, Ty)
tcBinOp f (e1, t1) (e2, t2) tf
  = do
      (e1', t1') <-tcE4 e1
      (e2', t2') <- tcE4 e2
      unless (t1 == t1')(incompatibleTypes t1 t1')
      unless (t2 == t2')(incompatibleTypes t2 t2')
      pure (f e1' e2', tf)

-- type checking monad interface

askEnv :: Var -> TcM Ty
askEnv v = do
  r <- gets (Map.lookup v . context)
  case r of
    Nothing -> undefinedVar v
    Just t -> pure t

addDecl :: Var -> Ty -> TcM ()
addDecl v t
  = modify (\ tcenv -> tcenv{  context = Map.insert v t (context tcenv) })

isDefinedVar :: Var -> TcM Bool
isDefinedVar v = gets (Map.member v . context)

withLocalEnv :: TcM a -> TcM a
withLocalEnv m
  = do
      env <- get
      r <- m
      put env
      pure r

-- type checking monad

type TcM a = ExceptT String (WriterT [String] (StateT TcEnv Identity)) a

type Ctx = Map Var Ty

data TcEnv
  = TcEnv {
      context :: Ctx
    }

initTcEnv :: TcEnv
initTcEnv = TcEnv Map.empty

runTcM :: TcEnv -> TcM a -> (((Either String a), [String]), TcEnv)
runTcM env m
  = runIdentity (runStateT (runWriterT (runExceptT m)) env)

-- error messages

wrapError :: Pretty b => TcM a -> b -> TcM a
wrapError m e
  = catchError m handler
    where
      handler msg = throwError (decorate msg)
      decorate msg = msg ++ "\n - in:\n" ++ pretty e

undefinedVar :: Var -> TcM a
undefinedVar v
  = throwError $ unwords ["Undefined variable:", pretty v]

alreadyDefinedVar :: Var -> TcM a
alreadyDefinedVar v
  = throwError $ unwords ["Variable already defined:", pretty v]

incompatibleTypes :: Ty -> Ty -> TcM a
incompatibleTypes expected found
  = throwError $ unlines ["Type error! Expected:", pretty expected, "but, found:", pretty found]
