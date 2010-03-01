module Tandoori.Ty.Ctxt (Ctxt(..), mkCtxt, getCon, getPolyVar, addPolyVar, addUserDecls, getUserDecl, removePolyVars, printCtxt, forceMonoVars) where

import Tandoori
import Tandoori.Ty
import Tandoori.Ty.ClassDecl
import Tandoori.Ty.ShowTy
import Tandoori.Ty.MonoEnv
import Tandoori.Ty.Pretty
import Tandoori.GHC.Internals
    
import Text.PrettyPrint.Tabulator    

import Tandoori.Ty.ShowTy
    
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
    
data Ctxt = Ctxt { polyVars :: Map.Map VarName (MonoEnv, TanType),
                   forcedMonoVars :: Set.Set VarName,
                   cons :: Map.Map ConName TanType,
                   classinfo :: ClassInfo,
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
                                
          rowsInfer = map (uncurry rowFromInfer) $ Map.toList $ polyVars c
          rowsDecl = map (uncurry rowFromDecl) $ map (\ (name, lty) ->  (name, unLoc lty)) $ Map.toList $ userdecls c

mkCtxt :: [(ConName, TanType)] -> ClassInfo -> Ctxt
mkCtxt cons classinfo = Ctxt { polyVars = Map.empty,
                               forcedMonoVars = Set.empty,
                               cons = conmap,
                               classinfo = classinfo,
                               userdecls = Map.empty }
    where conmap = Map.fromList cons
                   
isCon :: Ctxt -> ConName -> Bool
isCon c = flip Map.member (cons c)

getCon :: Ctxt -> ConName -> Maybe TanType
getCon c = flip Map.lookup (cons c)

getPolyVar :: Ctxt -> VarName -> Maybe (MonoEnv, TanType)
getPolyVar c = flip Map.lookup (polyVars c)

addPolyVar :: Ctxt -> VarName -> (MonoEnv, TanType) -> Ctxt
addPolyVar c name (m, ty) = c{polyVars = Map.insert name (m, ty) (polyVars c)}

forceMonoVars :: Ctxt -> Set.Set VarName -> Ctxt
forceMonoVars c ns = c{forcedMonoVars = (forcedMonoVars c) `Set.union` ns}
                            
addUserDecls :: Ctxt -> [LSig Name] -> Ctxt
addUserDecls c sigs = foldl addDecl c sigs
    where addDecl c (L srcloc (TypeSig (L _ name) (L _ ty))) = c {userdecls = Map.insert name (L srcloc ty) (userdecls c)}         
          addDecl c _                                        = c

getUserDecl :: Ctxt -> VarName -> Maybe (Located TanType)
getUserDecl c = flip Map.lookup (userdecls c)                                                    
                                                            
removePolyVars :: Ctxt -> [VarName] -> Ctxt
removePolyVars c names = c{polyVars = foldl removePolyVar (polyVars c) names}
    where removePolyVar = flip Map.delete                                                   
