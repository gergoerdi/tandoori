module Tandoori.CallGraph (sortDecls) where

import Tandoori
import Tandoori.Scope
import Language.Haskell.Syntax
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Graph as Graph    

callsFromDecl names (HsPatBind _ _ rhs _) = callsFromRhs names rhs
callsFromDecl names (HsFunBind matches) = Set.unions $ map (callsFromMatch names) matches
callsFromDecl names _ = Set.empty

callsFromMatch names (HsMatch _ _ _ rhs _) = callsFromRhs names rhs
                                           
callsFromRhs names (HsUnGuardedRhs expr) = callsFromExpr names expr
callsFromRhs names (HsGuardedRhss rhss) = Set.unions $ map (callsFromGuarded names) rhss

callsFromGuarded names (HsGuardedRhs _ guard expr) = Set.union (callsFromExpr names guard) (callsFromExpr names expr)
                                                   
callsFromExpr names (HsVar (UnQual name)) | Set.member name names = Set.singleton name
callsFromExpr names (HsInfixApp left op right) = error $ "Infix unsupported: " ++ unwords ((show op):map show [left, right]) -- TODO
callsFromExpr names (HsApp fun actual) = Set.union (callsFromExpr names fun) (callsFromExpr names actual)
callsFromExpr names (HsNegApp expr) = callsFromExpr names expr
callsFromExpr names (HsLet decls expr) = Set.union guardcalls $ callsFromExpr names' expr
    where names' = Set.difference names newnames
          newnames = Set.fromList $ concat $ map boundNamesOfDeclInt decls
          guardcalls = Set.empty -- TODO
callsFromExpr names (HsLambda _ pats expr) = callsFromExpr names' expr
    where names' = Set.difference names newnames
          newnames = Set.fromList $ concat $ map boundNamesOfPatInt pats
callsFromExpr names (HsIf cond thn els) = Set.unions $ map (callsFromExpr names) [cond, thn, els]
callsFromExpr names (HsList exprs) = Set.unions $ map (callsFromExpr names) exprs
callsFromExpr names (HsParen expr) = callsFromExpr names expr
callsFromExpr names _ = Set.empty

bindToDeclMap :: [HsDecl] -> Map.Map VarName (Integer, HsDecl)
bindToDeclMap decls = Map.fromList $ concat $ map flatten $ declToNames
    where declToNames = zip (addIndex decls) $ map boundNamesOfDeclExt decls
          flatten (decl, names) = map (\name -> (name, decl)) names

addIndex xs = zip [0..] xs
                                  
callgraph decls = Graph.graphFromEdges $ map toEdges $ addIndex decls
    where toEdges (i, decl) = (decl, i, Set.toList $ dependencies decl)
          dependencies decl = Set.map (\name -> fst $ fromJust $ Map.lookup name bindmap) $ callsFromDecl boundNames decl
          boundNames = Set.fromList $ Map.keys bindmap
          bindmap = bindToDeclMap decls              

sortDecls :: [HsDecl] -> [[HsDecl]]
sortDecls decls = map (map declFromVertex) $ reverse vss
    where declFromVertex v = let (decl, _, _) = fromVertex v in decl
          (g, fromVertex, toVertex) = callgraph decls
          sort (ds, processed) vertex = (unprocessed:ds, processed')
              where unprocessed = filter (\v -> not (Set.member v processed)) dependencies
                    processed' = Set.union processed $ Set.fromList unprocessed
                    dependencies = Graph.reachable g vertex
          vss = fst $ foldl sort ([], Set.empty) $ reverse $ Graph.topSort g
