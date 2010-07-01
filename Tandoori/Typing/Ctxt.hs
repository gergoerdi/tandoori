{-# LANGUAGE NamedFieldPuns #-}
module Tandoori.Typing.Ctxt (Ctxt, mkCtxt,
                             monoVars, addMonoVars,
                             polyVars, getPolyVar, addPolyVars,
                             userDecls, getUserDecl, addUserDecls) where

import Tandoori
import Tandoori.Typing
import Tandoori.Typing.MonoEnv
import Tandoori.GHC.Internals (Located)
    
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

data Ctxt = Ctxt { monoVars :: Set.Set VarName,
                   polyVars :: Map.Map VarName (MonoEnv, PolyTy),
                   userDecls :: Map.Map VarName (Located PolyTy) }
    
mkCtxt :: Ctxt
mkCtxt = Ctxt { monoVars = Set.empty,
                polyVars = Map.empty,
                userDecls = Map.empty }          
          
addMonoVars ctxt@Ctxt{monoVars} vars = ctxt{ monoVars = monoVars `Set.union` vars }

getPolyVar :: Ctxt -> VarName -> Maybe (MonoEnv, PolyTy)
getPolyVar Ctxt{polyVars} varname = Map.lookup varname polyVars
                                    
addPolyVars :: Ctxt -> [(VarName, (MonoEnv, PolyTy))] -> Ctxt
addPolyVars ctxt@Ctxt{polyVars} vars = ctxt{ polyVars = polyVars `Map.union` (Map.fromList vars) }

getUserDecl :: Ctxt -> VarName -> Maybe (Located PolyTy)
getUserDecl Ctxt{userDecls} varname = Map.lookup varname userDecls

addUserDecls :: Ctxt -> [(VarName, Located PolyTy)] -> Ctxt
addUserDecls ctxt@Ctxt{userDecls} binds = ctxt{userDecls = foldl addDecl userDecls binds}
    where addDecl decls (name, lτ) = Map.insert name lτ decls
