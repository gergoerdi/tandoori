module Tandoori.Ty.ClassDecl where

import Tandoori
import Tandoori.Ty   
import Tandoori.GHC.Internals
import Tandoori.Kludge.Show
import Tandoori.Ty.Canonize
    
import Data.Maybe    
import qualified Data.Graph as Graph    

sortClassDecls :: [TyClDecl Name] -> [TyClDecl Name]
sortClassDecls decls = map declFromVertex $ reverse $ Graph.topSort g
    where  declFromVertex v = let (decl, _, _) = fromVertex v in decl
           (g, fromVertex, toVertex) = inheritanceTree                                      
           decls' = filter isClassDecl decls
           inheritanceTree = Graph.graphFromEdges $ map toEdges decls'
           toEdges decl = (decl, tcdName decl, bases decl)
           bases decl = map baseFromPred $ map unLoc $ ctxt
               where  ctxt = unLoc $ tcdCtxt decl
                      baseFromPred (HsClassP cls [lty]) = cls          
                               
funsFromClassDecl :: TyClDecl Name -> ([LSig Name], LHsBinds Name)
funsFromClassDecl decl | isClassDecl decl = (map addClass lsigs, methodbag)
                       where classname = tcdName decl
                             preds = map unLoc $ unLoc $ tcdCtxt decl
                             lsigs = tcdSigs decl
                             methodbag = tcdMeths decl
                             [UserTyVar tv] = map unLoc $ tcdTyVars decl
                             addClass (L loc (TypeSig lname lty)) = L loc (TypeSig lname lty')
                                 where (ty, preds) = collectPredsTy (unLoc lty)
                                       preds' = (HsClassP classname [noLoc $ HsTyVar tv]):preds
                                       lty' = noLoc $ tyFromPreds ty preds'
