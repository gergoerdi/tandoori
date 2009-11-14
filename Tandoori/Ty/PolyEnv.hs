module Tandoori.Ty.PolyEnv where
--module Tandoori.Ty.PolyEnv (PolyEnv, emptyPoly, getCon, getPolyVar, addPolyVar, addUserDecls, getUserDecl, removePolyVars, isLocal, declareLocals) where

import Tandoori
import Tandoori.Ty
import Tandoori.Ty.MonoEnv
import qualified Data.Map as Map
import qualified Data.Set as Set

import SrcLoc
import HsBinds
import Name
    
data PolyEnv = PolyEnv { polyvarmap :: Map.Map VarName (MonoEnv, TanType),
                         conmap :: Map.Map ConName TanType,
                         locals :: Set.Set VarName,
                         scopelocals :: Set.Set VarName,
                         userdecls :: Map.Map VarName TanType
                       }

mkPoly :: [(ConName, TanType)] -> PolyEnv
mkPoly cons = PolyEnv{polyvarmap = Map.empty, conmap = conmap, locals = Set.empty, scopelocals = Set.empty, userdecls = Map.empty}
    where conmap = Map.fromList cons

isCon :: PolyEnv -> ConName -> Bool
isCon p = flip Map.member (conmap p)

getCon :: PolyEnv -> ConName -> Maybe TanType
getCon p = flip Map.lookup (conmap p)

getPolyVar :: PolyEnv -> VarName -> Maybe (MonoEnv, TanType)
getPolyVar p = flip Map.lookup (polyvarmap p)

addPolyVar :: PolyEnv -> VarName -> (MonoEnv, TanType) -> PolyEnv
addPolyVar p name typing = p{polyvarmap = Map.insert name typing (polyvarmap p)}

addUserDecls :: PolyEnv -> [Sig Name] -> PolyEnv
addUserDecls p sigs = foldl addDecl p sigs
    where addDecl p (TypeSig (L _ name) (L _ ty)) = p{userdecls = Map.insert name ty (userdecls p)}
          addDecl p _                             = p

getUserDecl :: PolyEnv -> VarName -> Maybe TanType
getUserDecl p = flip Map.lookup (userdecls p)                                                    
                                                            
removePolyVars :: PolyEnv -> [VarName] -> PolyEnv
removePolyVars p names = p{polyvarmap = foldl removePolyVar (polyvarmap p) names}
    where removePolyVar = flip Map.delete
                                                   
isLocal :: PolyEnv -> VarName -> Bool
isLocal p@PolyEnv{locals = locals} name = (Set.member name locals) || (isScopelocal p name)

isScopelocal :: PolyEnv -> VarName -> Bool                                                                   
isScopelocal PolyEnv{scopelocals = scopelocals} name = Set.member name scopelocals
                
newScope :: PolyEnv -> PolyEnv
newScope p = p {locals = (locals p) `Set.union` (scopelocals p), scopelocals = Set.empty }
                                        
declareLocals :: PolyEnv -> [VarName] -> PolyEnv
declareLocals p names = p{locals = (locals p) `Set.union` (Set.fromList names)}
                                                 
restrictScope :: PolyEnv -> MonoEnv -> MonoEnv
restrictScope p = filterMonoVars (\ name ty -> isScopelocal p name)
