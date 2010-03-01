module Tandoori.Ty.ClassDecl (mkClassGraph, sortClassDecls, mkClassInfo, ClassInfo, baseClassesOf, funsFromClassDecl, emptyClassInfo) where

import Tandoori
import Tandoori.Ty   
import Tandoori.GHC.Internals
import Tandoori.Ty.Canonize
    
import Data.Maybe    
import qualified Data.Graph as Graph    

data ClassInfo = ClassInfo { baseClassesOf :: Name -> [Name] }
newtype ClassGraph = CG (Graph.Graph,
                         Graph.Vertex -> (TyClDecl Name, Name, [Name]),
                         Name -> Graph.Vertex)

instance (Show ClassInfo) -- TODO: Kill
    where show ci = "ClassInfo"

declFromVertex (CG (_, fromVertex, _)) v = let (decl, _, _) = fromVertex v in decl
clsFromVertex (CG (_, fromVertex, _)) v = let (_, cls, _) = fromVertex v in cls

unknownCls cls = error $ unwords ["Unknown class", showSDoc (ppr cls)]                                                            
                                                            
mkClassGraph :: [TyClDecl Name] -> ClassGraph
mkClassGraph decls = CG (g, fromVertex, toVertex')
    where (g, fromVertex, toVertex) = Graph.graphFromEdges $ map toEdges decls'
          toVertex' cls = fromMaybe (unknownCls cls) $ toVertex cls
          decls' = filter isClassDecl decls
          toEdges decl = (decl, tcdName decl, baseclss decl)
          baseclss decl = map baseclsFromPred $ map unLoc $ ctxt
              where ctxt = unLoc $ tcdCtxt decl
                    baseclsFromPred (HsClassP cls [lty]) = cls

mkClassInfo :: ClassGraph -> ClassInfo                                                           
mkClassInfo cg = ClassInfo { baseClassesOf = baseClassesOf }
    where CG (g, fromVertex, toVertex) = cg
          baseClassesOf cls = filter (/= cls) $ map (clsFromVertex cg) $ Graph.reachable g (toVertex cls)

emptyClassInfo :: ClassInfo
emptyClassInfo = ClassInfo { baseClassesOf = \ cls -> unknownCls cls }
                              
                              
sortClassDecls :: ClassGraph -> [TyClDecl Name]
sortClassDecls cg@(CG (g, fromVertex, toVertex)) = map (declFromVertex cg) $ reverse $ Graph.topSort g
                               
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
