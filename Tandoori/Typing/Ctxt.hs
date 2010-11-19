{-# LANGUAGE NamedFieldPuns #-}
module Tandoori.Typing.Ctxt (Ctxt, mkCtxt,
                             monoVars, addMonoVars,
                             polyVars, getPolyVar, addPolyVars,
                             userDecls, getUserDecl, addUserDecls) where

import Tandoori
import Tandoori.Typing
import Tandoori.Typing.MonoEnv
import Tandoori.GHC.Internals (Located)
    
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Ctxt = Ctxt { monoVars :: Set VarName,
                   polyVars :: Map VarName (MonoEnv, Ty),
                   userDecls :: Map VarName (Located PolyTy) }
    
mkCtxt :: Ctxt
mkCtxt = Ctxt { monoVars = Set.empty,
                polyVars = Map.empty,
                userDecls = Map.empty }          
          
addMonoVars ctxt@Ctxt{monoVars} vars = ctxt{ monoVars = monoVars `Set.union` vars }

getPolyVar :: Ctxt -> VarName -> Maybe (MonoEnv, Ty)
getPolyVar Ctxt{polyVars} varname = Map.lookup varname polyVars
                                    
addPolyVars :: Ctxt -> [(VarName, (MonoEnv, Ty))] -> Ctxt
addPolyVars ctxt@Ctxt{polyVars} vars = ctxt{ polyVars = polyVars `Map.union` (Map.fromList vars) }

getUserDecl :: Ctxt -> VarName -> Maybe (Located PolyTy)
getUserDecl Ctxt{userDecls} varname = Map.lookup varname userDecls

addUserDecls :: Ctxt -> [(VarName, Located PolyTy)] -> Ctxt
addUserDecls ctxt@Ctxt{userDecls} binds = ctxt{userDecls = foldl addDecl userDecls binds}
    where addDecl decls (name, lσ) = Map.insert name lσ decls
