module Tandoori.Ty.Ctxt where
--module Tandoori.Ty.Ctxt (Ctxt, mkCtxt, getCon, getPolyVar, addPolyVar, addUserDecls, getUserDecl, removePolyVars, isLocal, declareLocals, newScope, restrictScope, printCtxt) where

import Tandoori
import Tandoori.Ty
import Tandoori.Ty.ShowTy
import Tandoori.Ty.MonoEnv
import Tandoori.Ty.Pretty
import Tandoori.GHC.Internals
    
import Text.PrettyPrint.Tabulator    

import Tandoori.Ty.ShowTy
    
import qualified Data.Map as Map
import qualified Data.Set as Set
    
data Ctxt = Ctxt { polyvars :: Map.Map VarName (MonoEnv, TanType),
                   cons :: Map.Map ConName TanType,
                   locals :: Set.Set VarName,
                   scopelocals :: Set.Set VarName,
                   userdecls :: Map.Map VarName (Located TanType) }
             
printCtxt :: Ctxt -> IO ()             
printCtxt c = do print $ tabTy (rowsDecl ++ rowsInfer)
                          
    where showNameShort qname = occNameString $ nameOccName qname
          showTy ty = show $ prettyTy ty
          --showTy ty = showSDocUnqual $ ppr $ prettyTy ty
                                
          rowFromInfer name (m, ty) = (showNameShort name, showTy ty)
          rowFromDecl name ty = (showNameShort name, showTy ty)

          rowTy (sname, sty) = [sname, "::", sty]
          tabTy rows = fromRows $ map rowTy rows
                                
          rowsInfer = map (uncurry rowFromInfer) $ Map.toList $ polyvars c
          rowsDecl = map (uncurry rowFromDecl) $ map (\ (name, lty) ->  (name, unLoc lty)) $ Map.toList $ userdecls c

mkCtxt :: [(ConName, TanType)] -> Ctxt
mkCtxt cons = Ctxt { polyvars = Map.empty,
                     cons = conmap,
                     locals = Set.empty,
                     scopelocals = Set.empty,
                     userdecls = Map.empty }
    where conmap = Map.fromList cons
                   
isCon :: Ctxt -> ConName -> Bool
isCon c = flip Map.member (cons c)

getCon :: Ctxt -> ConName -> Maybe TanType
getCon c = flip Map.lookup (cons c)

getPolyVar :: Ctxt -> VarName -> Maybe (MonoEnv, TanType)
getPolyVar c = flip Map.lookup (polyvars c)

addPolyVar :: Ctxt -> VarName -> (MonoEnv, TanType) -> Ctxt
addPolyVar c name (m, ty) = c{polyvars = Map.insert name (m, ty) (polyvars c)}

addUserDecls :: Ctxt -> [LSig Name] -> Ctxt
addUserDecls c sigs = foldl addDecl c sigs
    where --addDecl c (L srcloc (TypeSig (L _ name) (L _ ty))) = error $ show ty
          addDecl c (L srcloc (TypeSig (L _ name) (L _ ty))) = c {userdecls = Map.insert name (L srcloc ty) (userdecls c)}         
          addDecl c _                                        = c

getUserDecl :: Ctxt -> VarName -> Maybe (Located TanType)
getUserDecl c = flip Map.lookup (userdecls c)                                                    
                                                            
removePolyVars :: Ctxt -> [VarName] -> Ctxt
removePolyVars c names = c{polyvars = foldl removePolyVar (polyvars c) names}
    where removePolyVar = flip Map.delete
                                                   
isLocal :: Ctxt -> VarName -> Bool
isLocal c@Ctxt{locals = locals} name = (Set.member name locals) || (isScopelocal c name)

isScopelocal :: Ctxt -> VarName -> Bool                                                                   
isScopelocal Ctxt{scopelocals = scopelocals} name = Set.member name scopelocals
                
newScope :: Ctxt -> Ctxt
newScope c = c {locals = (locals c) `Set.union` (scopelocals c), scopelocals = Set.empty }
                                        
declareLocals :: Ctxt -> [VarName] -> Ctxt
declareLocals c names = c{locals = (locals c) `Set.union` (Set.fromList names)}
                                                 
restrictScope :: Ctxt -> MonoEnv -> MonoEnv
restrictScope c = filterMonoVars (\ name ty -> isScopelocal c name)
