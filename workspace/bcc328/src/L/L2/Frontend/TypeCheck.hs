module L.L2.Frontend.TypeCheck where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.List ((\\))

import L.L2.Frontend.Syntax
import Utils.Var

typeCheck :: L2 -> Either String L2
typeCheck = error "Not implemented!"

-- basic monad infrastructure

type TcM a = ExceptT String (WriterT [String] (StateT TcEnv Identity)) a

data TcEnv
  = TcEnv {
      context :: [Var] -- imutable variable list
    }

initTcEnv :: TcEnv
initTcEnv = TcEnv []

insertVar :: Var -> TcM ()
insertVar v = modify (\ env -> env{context = v : context env})

removeVar :: Var -> TcM ()
removeVar v = modify (\ env -> env {context = (context env) \\ [v]})

runTcM :: TcEnv -> TcM a -> (((Either String a), [String]), TcEnv)
runTcM env m
  = runIdentity (runStateT (runWriterT (runExceptT m)) env)


