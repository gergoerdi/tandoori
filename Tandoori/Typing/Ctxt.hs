{-# LANGUAGE NamedFieldPuns #-}
module Tandoori.Typing.Ctxt (Ctxt, mkCtxt,
                             printCtxt,
                             monoVars, addMonoVars,
                             getPolyVar, addPolyVars,
                             getUserDecl, addUserDecls) where

import Tandoori
import Tandoori.Typing
import Tandoori.Typing.ShowTy
import Tandoori.Typing.MonoEnv
import Tandoori.Typing.Pretty
import Tandoori.GHC.Internals
    
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

printCtxt :: Ctxt -> IO ()
printCtxt c = mapM_  print $ ((map (\ (name, (m, σ)) -> (name, σ)) $ Map.toList $ polyVars c) ++
                              (map (\ (name, (L _ σ)) -> (name, σ)) $ Map.toList $ userDecls c))
    
-- printCtxt :: Ctxt -> IO ()             
-- printCtxt c = do print $ tabTy (rowsDecl ++ rowsInfer)
                          
--     where showNameShort qname = occNameString $ nameOccName qname
--           showTy ty = show $ prettyTy ty
--           showCTy cty = show $ prettyTy $ uncanonize cty
--           --showTy ty = showSDocUnqual $ ppr $ prettyTy ty
                                
--           rowFromInfer name (m, cty) = (showNameShort name, showCTy cty)
--           rowFromDecl name cty = (showNameShort name, showCTy cty)

--           rowTy (sname, sty) = [sname, "::", sty]
--           tabTy rows = fromRows $ map rowTy rows
                                
--           rowsInfer = map (uncurry rowFromInfer) $ Map.toList $ polyVars c
--           rowsDecl = map (uncurry rowFromDecl) $ map (\ (name, lty) ->  (name, unLoc lty)) $ Map.toList $ userDecls c

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
