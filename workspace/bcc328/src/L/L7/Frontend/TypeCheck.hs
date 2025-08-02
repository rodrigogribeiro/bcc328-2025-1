module L.L7.Frontend.TypeCheck where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans

import Data.Map (Map)
import qualified Data.Map as Map

import L.L7.Frontend.SCCAnalysis
import L.L7.Frontend.Syntax

import Utils.Pretty
import Utils.Value
import Utils.Var

-- type checking top-level function

typeCheck :: L7 -> Either String [(Var, Ty)]
typeCheck l7
  = runTcM Map.empty (tcProgram l7')
    where
      l7' = dependencyAnalysis l7

-- typing programs

tcProgram :: L7 -> TcM [(Var, Ty)]
tcProgram (L7 ds)
  = do
       env <- tcFuns ds
       pure env
  where
    tcFuns [] = pure []
    tcFuns (d : ds)
      = do
          (v,t) <- tcFunDef d
          extEnv v t
          ((v, t) :) <$> tcFuns ds
    hasMain env = any (\ (v, t) -> v == (Var "main")) env

-- typing functions

tcFunDef :: D7 -> TcM (Var, Ty)
tcFunDef d@(Fun v ps t ss)
  = do
      let env = map paramTuple ps
          (vs, ts) = unzip env
          ty = funtype ts t
          env' = (v, ty) : env
      rt <- withExtendedCtx env' (tcBlock ss)
      unless (rt == t) (incompatibleTypes t rt d)
      pure (v, ty)

paramTuple :: Param -> (Var, Ty)
paramTuple (Param v t) = (v, t)

funtype :: [Ty] -> Ty -> Ty
funtype ts t = foldr TyArrow t ts

-- typing statements

tcStmt :: S7 -> TcM Ty
tcStmt s@(SLet v ty e)
  = do
      ty' <- tcExp e
      unless (ty == ty') (incompatibleTypes ty ty' s)
      extEnv v ty
      pure TyUnit
tcStmt s@(SAssign v e)
  = do
      ty <- askCtx v
      ty' <- tcExp e
      unless (ty == ty') (incompatibleTypes ty ty' s)
      pure TyUnit
tcStmt s@(SRead e v)
  = do
      _ <- askCtx v
      ty <- tcExp e
      unless (ty == TyString) (incompatibleTypes TyString ty s)
      pure TyUnit
tcStmt s@(SPrint e)
  = do
      ty <- tcExp e
      unless (ty == TyString) (incompatibleTypes TyString ty s)
      pure TyUnit
tcStmt s@(SIf e ss1 ss2)
  = do
      ty <- tcExp e
      unless (ty == TyBool) (incompatibleTypes TyBool ty s)
      t1 <- tcBlock ss1
      t2 <- tcBlock ss2
      unless (t1 == t2) (incompatibleTypes t1 t2 s)
      pure t1
tcStmt s@(SWhile e ss)
  = do
      ty <- tcExp e
      unless (ty == TyBool) (incompatibleTypes TyBool ty s)
      tcBlock ss
tcStmt (SReturn e) = tcExp e

tcBlock :: [S7] -> TcM Ty
tcBlock [] = pure TyUnit
tcBlock [s] = tcStmt s
tcBlock (s@(SReturn _) : _)
  = throwError $ "Illegal return statement:" ++ pretty s
tcBlock (s : ss)
  = do
      _ <- tcStmt s
      tcBlock ss

-- typing expressions

tcExp :: E7 -> TcM Ty
tcExp (EValue v) = tcValue v
tcExp (EVar v _) = askCtx v
tcExp (EAdd e1 e2)
  = tcBinOp (e1, TyInt) (e2, TyInt) TyInt
tcExp (EMinus e1 e2)
  = tcBinOp (e1, TyInt) (e2, TyInt) TyInt
tcExp (EMult e1 e2)
  = tcBinOp (e1, TyInt) (e2, TyInt) TyInt
tcExp (EDiv e1 e2)
  = tcBinOp (e1, TyInt) (e2, TyInt) TyInt
tcExp (ELt e1 e2)
  = tcBinOp (e1, TyInt) (e2, TyInt) TyBool
tcExp e@(EEq e1 e2)
  = do
      t1 <- tcExp e1
      t2 <- tcExp e2
      unless (t1 == t2) (incompatibleTypes t1 t2 e)
      pure TyBool
tcExp (EAnd e1 e2)
  = tcBinOp (e1, TyBool) (e2, TyBool) TyBool
tcExp e@(ENot e1)
  = do
      t1 <- tcExp e1
      unless (t1 == TyBool) (incompatibleTypes TyBool t1 e)
      pure TyBool
tcExp e@(ECat e1 e2)
  = tcBinOp (e1, TyString) (e2, TyString) TyString
tcExp e@(ESize e1)
  = do
      t1 <- tcExp e1
      unless (t1 == TyString) (incompatibleTypes TyString t1 e)
      pure TyInt
tcExp e@(EI2S e1)
  = do
      t1 <- tcExp e1
      unless (t1 == TyInt) (incompatibleTypes TyInt t1 e)
      pure TyString
tcExp e@(EI2B e1)
  = do
      t1 <- tcExp e1
      unless (t1 == TyInt) (incompatibleTypes TyInt t1 e)
      pure TyBool
tcExp e@(ES2I e1)
  = do
      t1 <- tcExp e1
      unless (t1 == TyString) (incompatibleTypes TyString t1 e)
      pure TyInt
tcExp e@(ES2B e1)
  = do
      t1 <- tcExp e1
      unless (t1 == TyString) (incompatibleTypes TyString t1 e)
      pure TyBool
tcExp e@(EB2S e1)
  = do
      t1 <- tcExp e1
      unless (t1 == TyBool) (incompatibleTypes TyBool t1 e)
      pure TyString
tcExp e@(EB2I e1)
  = do
      t1 <- tcExp e1
      unless (t1 == TyBool) (incompatibleTypes TyBool t1 e)
      pure TyInt
tcExp e@(SCall f es)
  = do
      fty <- askCtx f
      ts' <- mapM tcExp es
      let (ts, t) = splitTy fty
          diff = filter (\ (t1, t1', e) -> t1 /= t1') (zip3 ts ts' es)
      unless (null diff) (callError f diff)
      pure t

splitTy :: Ty -> ([Ty], Ty)
splitTy (TyArrow t1 t2)
  = let (ts1, t2') = splitTy t2
    in (t1 : ts1, t2')
splitTy t = ([], t)

-- typing binary operator

tcBinOp :: (E7, Ty) -> (E7, Ty) -> Ty -> TcM Ty
tcBinOp (e1, t1) (e2, t2) rt
  = do
      t1' <- tcExp e1
      t2' <- tcExp e2
      unless (t1 == t1') (incompatibleTypes t1 t1' e1)
      unless (t2 == t2') (incompatibleTypes t2 t2' e2)
      pure rt

-- typing values

tcValue :: Value -> TcM Ty
tcValue (VInt _) = pure TyInt
tcValue (VBool _) = pure TyBool
tcValue (VStr _) = pure TyString
tcValue VUnit = pure TyUnit

-- definition of typing context

type Ctx = Map Var Ty

-- definition of a type checking monad

type TcM a = ExceptT String (StateT Ctx Identity) a

runTcM :: Ctx -> TcM a -> Either String a
runTcM ctx m
  = case runIdentity (runStateT (runExceptT m) ctx) of
      (Left err, _) -> Left err
      (Right r, _) -> Right r

askCtx :: Var -> TcM Ty
askCtx v
  = do
      ctx <- get
      case Map.lookup v ctx of
        Just ty -> pure ty
        Nothing -> undefinedVariableError v

extEnv :: Var -> Ty -> TcM ()
extEnv v ty = modify (Map.insert v ty)

withExtendedCtx :: [(Var, Ty)] -> TcM a -> TcM a
withExtendedCtx es m
  = do
      oldCtx <- get
      mapM_ (uncurry extEnv) es
      r <- m
      put oldCtx
      pure r

withLocalCtx :: TcM a -> TcM a
withLocalCtx m
  = do
      oldCtx <- get
      r <- m
      put oldCtx
      pure r

-- error messages

callError :: Var -> [(Ty, Ty, E7)] -> TcM a
callError v es
  = throwError $ unlines ["Incompatible arguments:"
                         , msg
                         , "in calling function:" ++ pretty v]
  where
    msg = unlines (map f es)
    f (exp, inf, e) = unlines [ "Expected:" ++ pretty exp
                              , "Found:" ++ pretty inf
                              , "in:" ++ pretty e]

incompatibleTypes :: Pretty a => Ty -> Ty -> a -> TcM b
incompatibleTypes exp fnd loc
  = throwError $ unlines [ "Incompatible types:"
                         , "Expected:" ++ pretty exp
                         , "Found:" ++ pretty fnd
                         , "at:" ++ pretty loc
                         ]

undefinedVariableError :: Var -> TcM a
undefinedVariableError v
  = throwError $ "Undefined variable:" ++ pretty v

undefinedMain :: TcM a
undefinedMain
  = throwError "Code does not have a main function!"
