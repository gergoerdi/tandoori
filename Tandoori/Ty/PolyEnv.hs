module Tandoori.Ty.PolyEnv where
--module Tandoori.Ty.PolyEnv (PolyEnv, emptyPoly, getCon, getPolyVar, addPolyVar, removePolyVars, isLocal, declareLocals) where

import Tandoori
import Tandoori.Ty
import Tandoori.Ty.MonoEnv
import qualified Data.Map as Map
import qualified Data.Set as Set

data PolyEnv = PolyEnv { polyvarmap :: Map.Map VarName (MonoEnv, TanType),
                         conmap :: Map.Map ConName TanType,
                         locals :: Set.Set VarName,
                         scopelocals :: Set.Set VarName
                       }

emptyPoly :: PolyEnv
emptyPoly = PolyEnv{polyvarmap = Map.empty, conmap = Map.empty, locals = Set.empty, scopelocals = Set.empty}

isCon :: PolyEnv -> ConName -> Bool
isCon PolyEnv{conmap = conmap} name = Map.member name conmap

getCon :: PolyEnv -> ConName -> Maybe TanType
getCon PolyEnv{conmap = conmap} name = Map.lookup name conmap

getPolyVar :: PolyEnv -> VarName -> Maybe (MonoEnv, TanType)
getPolyVar PolyEnv{polyvarmap = polyvarmap} name = Map.lookup name polyvarmap

addPolyVar :: PolyEnv -> VarName -> (MonoEnv, TanType) -> PolyEnv
addPolyVar p@PolyEnv{polyvarmap = polyvarmap} name typing = p{polyvarmap = Map.insert name typing polyvarmap}
                                                   
removePolyVars :: PolyEnv -> [VarName] -> PolyEnv
removePolyVars p@PolyEnv{polyvarmap = polyvarmap} names = p{polyvarmap = foldl removePolyVar polyvarmap names}
    where removePolyVar = flip Map.delete
                                                   
isLocal :: PolyEnv -> VarName -> Bool
isLocal p@PolyEnv{locals = locals} name = (Set.member name locals) || (isScopelocal p name)

isScopelocal :: PolyEnv -> VarName -> Bool                                                                   
isScopelocal PolyEnv{scopelocals = scopelocals} name = Set.member name scopelocals

                
newScope :: PolyEnv -> PolyEnv
newScope p@PolyEnv{locals = locals, scopelocals = scopelocals} = p {locals = locals `Set.union` scopelocals, scopelocals = Set.empty }
                                        
declareLocals :: PolyEnv -> [VarName] -> PolyEnv
declareLocals p@PolyEnv{locals = locals} names = p{locals = locals `Set.union` (Set.fromList names)}
                                                 
restrictScope :: PolyEnv -> MonoEnv -> MonoEnv
restrictScope p = filterMonoVars (\ name ty -> isScopelocal p name)
