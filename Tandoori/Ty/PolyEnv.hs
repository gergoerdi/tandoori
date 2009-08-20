module Tandoori.Ty.PolyEnv (PolyEnv, emptyPoly, getCon, getPolyVar, addPolyVar, removePolyVars, isLocal, declareLocals) where

import Tandoori
import Tandoori.Ty
import Tandoori.Ty.MonoEnv
import Language.Haskell.Syntax
import qualified Data.Map as Map
import qualified Data.Set as Set
    
data PolyEnv = PolyEnv { polyvarmap :: Map.Map VarName (MonoEnv, HsType),
                         conmap :: Map.Map ConName HsType,
                         locals :: Set.Set VarName
                       } deriving Show

emptyPoly :: PolyEnv
emptyPoly = PolyEnv{polyvarmap = Map.empty, conmap = Map.empty, locals = Set.empty}


getCon :: PolyEnv -> ConName -> Maybe HsType
getCon PolyEnv{conmap = conmap} name = Map.lookup name conmap

getPolyVar :: PolyEnv -> VarName -> Maybe (MonoEnv, HsType)
getPolyVar PolyEnv{polyvarmap = polyvarmap} name = Map.lookup name polyvarmap

addPolyVar :: PolyEnv -> VarName -> (MonoEnv, HsType) -> PolyEnv
addPolyVar p@PolyEnv{polyvarmap = polyvarmap} name typing = p{polyvarmap = Map.insert name typing polyvarmap}
                                                   
removePolyVars :: PolyEnv -> [VarName] -> PolyEnv
removePolyVars p@PolyEnv{polyvarmap = polyvarmap} names = p{polyvarmap = foldl removePolyVar polyvarmap names}
    where removePolyVar = flip Map.delete
                                                   
isLocal :: PolyEnv -> VarName -> Bool
isLocal PolyEnv{locals = locals} name = Set.member name locals

-- TODO: check for uniqueness
declareLocals :: PolyEnv -> [VarName] -> PolyEnv
declareLocals p@PolyEnv{locals = locals} names = p{locals = locals `Set.union` (Set.fromList names)}
