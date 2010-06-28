{-# LANGUAGE NamedFieldPuns #-}
module Tandoori.Ty.ClassDecl (BaseClasses, MethodInfo(..), getClassInfo) where

import Tandoori
import Tandoori.Ty   
import Tandoori.GHC.Internals
import Tandoori.Ty.Canonize
    
import Data.Maybe    
import qualified Data.Graph as Graph    

--- Public interface
---
data MethodInfo = MethodInfo { methodDecls :: [LSig Name],
                               methodImpls :: LHsBinds Name }
type BaseClasses = Name -> [Name]    
               
getClassInfo :: [LTyClDecl Name] -> ([MethodInfo], BaseClasses)
getClassInfo ldecls = (methods, baseClasses)
    where decls = map unLoc ldecls
          cg@CG{graph, toVertex} = mkClassGraph decls
          methods = map (uncurry MethodInfo . funsFromClassDecl) $ sortClassDecls cg
          baseClasses cls = filter (/= cls) $ map (clsFromVertex cg) $ Graph.reachable graph (toVertex cls)


--- Implementation
---
data ClassGraph = CG {graph :: Graph.Graph,
                      fromVertex :: Graph.Vertex -> (TyClDecl Name, Name, [Name]),
                      toVertex :: Name -> Graph.Vertex }

declFromVertex CG{fromVertex} v = let (decl, _, _) = fromVertex v in decl
clsFromVertex CG{fromVertex} v = let (_, cls, _) = fromVertex v in cls

unknownCls cls = error $ unwords ["Unknown class", showSDoc (ppr cls)]                                                            
                                                            
mkClassGraph :: [TyClDecl Name] -> ClassGraph
mkClassGraph decls = CG g fromVertex toVertex'
    where (g, fromVertex, toVertex) = Graph.graphFromEdges $ map toEdges decls'
          toVertex' cls = fromMaybe (unknownCls cls) $ toVertex cls
          decls' = filter isClassDecl decls
          toEdges decl = (decl, tcdName decl, baseclss decl)
          baseclss decl = map baseclsFromPred $ map unLoc $ ctxt
              where ctxt = unLoc $ tcdCtxt decl
                    baseclsFromPred (HsClassP cls [lty]) = cls
                                                           
sortClassDecls :: ClassGraph -> [TyClDecl Name]
sortClassDecls cg = map (declFromVertex cg) $ reverse $ Graph.topSort $ graph cg
                               
funsFromClassDecl :: TyClDecl Name -> ([LSig Name], LHsBinds Name)
funsFromClassDecl decl | isClassDecl decl = (map addClass lsigs, methodbag)
                       where classname = tcdName decl
                             preds = map unLoc $ unLoc $ tcdCtxt decl
                             lsigs = tcdSigs decl
                             methodbag = tcdMeths decl
                             [UserTyVar tv] = map unLoc $ tcdTyVars decl
                             addClass (L loc (TypeSig lname lty)) = L loc (TypeSig lname lty')
                                 where cty = canonize (unLoc lty)
                                       pred = HsClassP classname [noLoc $ HsTyVar tv]
                                       cty' = addPred cty (noLoc pred)
                                       lty' = noLoc $ uncanonize cty'
