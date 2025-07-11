module L.L3.Frontend.TypeCheck where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.Map (Map)
import qualified Data.Map as Map

import L.L3.Frontend.Syntax

import Utils.Pretty
import Utils.Value
import Utils.Var

-- type checking algorithm

typeCheck :: L3 -> Either String L3
typeCheck l3 = fst $ fst $ runTcM initTcEnv (tcL3 l3)

-- typing L3 programs

tcL3 :: L3 -> TcM L3
tcL3 (L3 ss) = L3 <$> mapM tcS3 ss

-- typing L3 statements

tcS3 :: S3 -> TcM S3
tcS3 (SLet v t e)
  = do
      v_defined <- isDefinedVar v
      when v_defined (undefinedVar v)
      (e', t') <- tcE3 e
      unless (t == t') (incompatibleTypes t t')
      addDecl v t
      pure (SLet v t e')
tcS3 (SAssign v e)
  = do
      t <- askEnv v
      (e', t') <- tcE3 e
      unless (t == t') (incompatibleTypes t t')
      pure (SAssign v e')
tcS3 (SRead e v)
  = do
      t <- askEnv v
      (e', t') <- tcE3 e
      unless (t' == TString) (incompatibleTypes TString t')
      pure (SRead e' v)
tcS3 (SPrint e)
  = do
      (e', t) <- tcE3 e
      unless (t == TString) (incompatibleTypes TString t)
      pure (SPrint e)

-- typing L3 expressions

tcE3 :: E3 -> TcM (E3, Ty)
tcE3 (EValue v)
  = do
      t <- tcValue v
      pure (EValue v, t)
tcE3 (EVar v Nothing)
  = do
      t <- askEnv v
      pure (EVar v (Just t), t)
tcE3 (EAdd e1 e2)
  = tcBinOp EAdd (e1, TInt) (e2, TInt) TInt
tcE3 (EMinus e1 e2)
  = tcBinOp EMinus (e1, TInt) (e2, TInt) TInt
tcE3 (EMult e1 e2)
  = tcBinOp EMult (e1, TInt) (e2, TInt) TInt
tcE3 (EDiv e1 e2)
  = tcBinOp EDiv (e1, TInt) (e2, TInt) TInt
tcE3 (EAnd e1 e2)
  = tcBinOp EAnd (e1, TBool) (e2, TBool) TBool
tcE3 (ECat e1 e2)
  = tcBinOp ECat (e1, TString) (e2, TString) TString
tcE3 (ENot e1)
  = do
      (e', t) <- tcE3 e1
      unless (t == TBool)(incompatibleTypes t TBool)
      pure (ENot e', TBool)
tcE3 (ESize e1)
  = do
      (e', t) <- tcE3 e1
      unless (t == TString)(incompatibleTypes t TString)
      pure (ESize e', TInt)
tcE3 (ELt e1 e2)
  = do
      (e1', t1) <- tcE3 e1
      (e2', t2) <- tcE3 e2
      unless (t1 == t2) (incompatibleTypes t1 t2)
      pure (ELt e1' e2', TBool)
tcE3 (EEq e1 e2)
  = do
      (e1', t1) <- tcE3 e1
      (e2', t2) <- tcE3 e2
      unless (t1 == t2) (incompatibleTypes t1 t2)
      pure (EEq e1' e2', TBool)
tcE3 (EI2S e1)
  = do
      (e1', t) <- tcE3 e1
      unless (t == TInt) (incompatibleTypes t TInt)
      pure (EI2S e1', TString)
tcE3 (EI2B e1)
  = do
      (e1', t) <- tcE3 e1
      unless (t == TInt) (incompatibleTypes t TInt)
      pure (EI2S e1', TBool)
tcE3 (ES2I e1)
  = do
      (e1', t) <- tcE3 e1
      unless (t == TString) (incompatibleTypes t TString)
      pure (EI2S e1', TInt)
tcE3 (ES2B e1)
  = do
      (e1', t) <- tcE3 e1
      unless (t == TString) (incompatibleTypes t TString)
      pure (ES2B e1', TBool)
tcE3 (EB2S e1)
  = do
      (e1', t) <- tcE3 e1
      unless (t == TBool) (incompatibleTypes t TBool)
      pure (EB2S e1', TString)
tcE3 (EB2I e1)
  = do
      (e1', t) <- tcE3 e1
      unless (t == TBool) (incompatibleTypes t TBool)
      pure (EI2S e1', TInt)

-- typing L3 values

tcValue :: Value -> TcM Ty
tcValue (VInt _) = pure TInt
tcValue (VStr _) = pure TString
tcValue (VBool _) = pure TBool

-- typing binary operators

tcBinOp :: (E3 -> E3 -> E3) ->
           (E3, Ty) ->
           (E3, Ty) ->
           Ty -> TcM (E3, Ty)
tcBinOp f (e1, t1) (e2, t2) tf
  = do
      (e1', t1') <-tcE3 e1
      (e2', t2') <- tcE3 e2
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
