module Tandoori.Typing.ClassDecl (classMap) where

import Tandoori.Typing
import Tandoori.Typing.Monad
import Tandoori.Typing.Error
import Tandoori.GHC.Internals
import Tandoori.Typing.Repr

import Control.Monad.Error
import Control.Applicative

import qualified Data.Map as Map    
import qualified Data.Graph as G
import qualified Data.Tree as T
    
-- TODO: move to separate module
classMap :: [TyClDecl Name] -> Typing [(Cls, ClsInfo)]
classMap decls = do (g, fromVertex) <- classGraph decls
                    let fromVertex' = tcdName . fromVertex
                        components = G.scc g
                        checkComponent tree = case map fromVertex' (T.flatten tree) of
                                                [c] -> return c
                                                cs -> raiseError $ ClassCycle cs
                    mapM_ checkComponent components
                    let toClassInfo v = do let decl = fromVertex v
                                               cls = tcdName decl
                                               [L _ (UserTyVar α _)] = tcdTyVars decl
                                               lsigs = tcdSigs decl
                                               supers = map fromVertex' $ G.reachable g v
                                           meths <- mapM methDecl lsigs
                                           return (cls, ClsInfo supers α (Map.fromList meths))
                    -- TODO: Check uniqueness of member names
                    mapM toClassInfo $ G.vertices g
    where methDecl (L loc (TypeSig (L _ name) (L _ ty))) = do σ <- fromHsType ty
                                                              return (name, (L loc σ))

classGraph :: [TyClDecl Name] -> Typing (G.Graph, G.Vertex -> TyClDecl Name)
classGraph decls = do (g, fromVertex, toVertex) <- G.graphFromEdges <$> edges
                      let clsFromVertex v = let (cls, _, _) = fromVertex v in cls
                      return (g, clsFromVertex)
    where decls' = filter isClassDecl decls
          edges = mapM edgesFromDecl decls'                  
          edgesFromDecl decl = do checkCtx
                                  return (decl, cls, map fst ctx)
              where cls = tcdName decl
                    [L _ (UserTyVar α _)] = tcdTyVars decl
                    ctx = map superFromPred $ map unLoc $ unLoc $ tcdCtxt decl
                    superFromPred (HsClassP cls [L _ (HsTyVar α')]) = (cls, α')
                    checkCtx = forM_ ctx $ \ (cls', α') ->
                                 unless (α' == α) $
                                   raiseError $ InvalidClassCtx (cls, α) (cls', α')
                                              
                             
