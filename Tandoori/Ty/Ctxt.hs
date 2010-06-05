{-# LANGUAGE NamedFieldPuns #-}
module Tandoori.Ty.Ctxt (Ctxt(src, loc), mkCtxt, printCtxt,
                         monoVars, addMonoVars,
                         getPolyVar, addPolyVars,
                         getUserDecl, addUserDecls)
    where

import Tandoori
import Tandoori.Ty
import Tandoori.Ty.ClassDecl
import Tandoori.Ty.Canonize
import Tandoori.Ty.ShowTy
import Tandoori.Ty.MonoEnv
import Tandoori.Ty.Pretty
import Tandoori.GHC.Internals
import Tandoori.Errors
    
import Text.PrettyPrint.Tabulator    

import Tandoori.Ty.ShowTy
    
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
    
printCtxt :: Ctxt -> IO ()             
printCtxt c = do print $ tabTy (rowsDecl ++ rowsInfer)
                          
    where showNameShort qname = occNameString $ nameOccName qname
          showTy ty = show $ prettyTy ty
          showCTy cty = show $ prettyTy $ uncanonize cty
          --showTy ty = showSDocUnqual $ ppr $ prettyTy ty
                                
          rowFromInfer name (m, cty) = (showNameShort name, showCTy cty)
          rowFromDecl name cty = (showNameShort name, showCTy cty)

          rowTy (sname, sty) = [sname, "::", sty]
          tabTy rows = fromRows $ map rowTy rows
                                
          rowsInfer = map (uncurry rowFromInfer) $ Map.toList $ polyVars c
          rowsDecl = map (uncurry rowFromDecl) $ map (\ (name, lty) ->  (name, unLoc lty)) $ Map.toList $ userDecls c

mkCtxt :: Ctxt
mkCtxt = Ctxt { loc = noSrcSpan,
                src = Nothing,
                monoVars = Set.empty,
                polyVars = Map.empty,
                userDecls = Map.empty }

data Ctxt = Ctxt { loc :: SrcSpan,
                   src :: Maybe ErrorSource,
                   monoVars :: Set.Set VarName,
                   polyVars :: Map.Map VarName (MonoEnv, CanonizedType),
                   userDecls :: Map.Map VarName (Located CanonizedType) }
    
          
          
addMonoVars ctxt@Ctxt{monoVars} vars = ctxt{ monoVars = monoVars `Set.union` vars }

getPolyVar :: Ctxt -> VarName -> Maybe (MonoEnv, CanonizedType)
getPolyVar Ctxt{polyVars} varname = Map.lookup varname polyVars
                                    
addPolyVars :: Ctxt -> [(VarName, (MonoEnv, CanonizedType))] -> Ctxt
addPolyVars ctxt@Ctxt{polyVars} vars = ctxt{ polyVars = polyVars `Map.union` (Map.fromList vars) }

getUserDecl :: Ctxt -> VarName -> Maybe (Located CanonizedType)
getUserDecl Ctxt{userDecls} varname = Map.lookup varname userDecls

addUserDecls :: Ctxt -> [LSig Name] -> Ctxt
addUserDecls ctxt@Ctxt{userDecls} sigs = ctxt{ userDecls = foldl addDecl userDecls sigs }
    where addDecl decls (L srcloc (TypeSig (L _ name) (L _ ty))) = Map.insert name (L srcloc $ canonize ty) decls
          addDecl decls _                                        = decls
