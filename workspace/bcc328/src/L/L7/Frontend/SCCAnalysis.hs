module L.L7.Frontend.SCCAnalysis where

import Algebra.Graph.AdjacencyMap
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as N
import Algebra.Graph.AdjacencyMap.Algorithm

import Data.List
import Data.List.NonEmpty (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import L.L7.Frontend.Syntax

import Utils.Var

-- top level dependency analysis function

dependencyAnalysis :: L7 -> L7
dependencyAnalysis (L7 ds) = L7 (analysis ds)

analysis :: [D7] -> [D7]
analysis ds
  =  let grph = mkGraph ds
         cmps = scc grph
     in case topSort cmps of
          Left _ -> []
          Right ds' -> reverse $ concatMap (toList . N.vertexList1) ds'


-- building the dependency graph

mkGraph :: [D7] -> AdjacencyMap D7
mkGraph ds = stars $ mkEdges (mkDeclEnv ds) ds

mkEdges :: DeclEnv -> [D7] -> [(D7,[D7])]
mkEdges env = foldr step []
  where
    step d ac = (d, lookupDecls (called d) env) : ac

-- building a function environment

type DeclEnv = Map Var D7

lookupDecl :: Var -> DeclEnv -> Maybe D7
lookupDecl v env = Map.lookup v env


lookupDecls :: [Var] -> DeclEnv -> [D7]
lookupDecls ns env = mapMaybe (flip lookupDecl env) ns


mkDeclEnv :: [D7] -> DeclEnv
mkDeclEnv
  = foldr go Map.empty
    where
      go d@(Fun n _ _ _) ac = Map.insert n d ac

-- getting function calls

class Calls a where
  called :: a -> [Var]

instance Calls a => Calls [a] where
  called = foldr (union . called) []

instance Calls D7 where
  called (Fun _ _ _ ss) = called ss

instance Calls S7 where
  called (SLet _ _ e) = called e
  called (SAssign _ e) = called e
  called (SRead e _) = called e
  called (SPrint e) = called e
  called (SIf e bt be)
    = called e `union` called (bt ++ be)
  called (SWhile e bw)
    = called e `union` called bw
  called (SReturn e)
    = called e

instance Calls E7 where
  called (EAdd e1 e2) = called [e1, e2]
  called (EMinus e1 e2) = called [e1, e2]
  called (EMult e1 e2) = called [e1, e2]
  called (EDiv e1 e2) = called [e1, e2]
  called (ELt e1 e2) = called [e1, e2]
  called (EEq e1 e2) = called [e1, e2]
  called (EAnd e1 e2) = called [e1, e2]
  called (ENot e) = called e
  called (ECat e1 e2) = called [e1, e2]
  called (ESize e1) = called e1
  called (EI2S e) = called e
  called (EI2B e) = called e
  called (EB2S e) = called e
  called (EB2I e) = called e
  called (ES2B e) = called e
  called (ES2I e) = called e
  called (SCall v es) = v : called es
  called _ = []

