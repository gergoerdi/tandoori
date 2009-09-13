module Tandoori.Ty.Infer(infer) where

import Tandoori
import Tandoori.Test
import Tandoori.State    
import Tandoori.Errors
import Tandoori.Ty
import Tandoori.Ty.MonoEnv
import Tandoori.Ty.PolyEnv
import Tandoori.Ty.Unify
import Tandoori.Ty.Printer
import Tandoori.Ty.Instantiate
import Tandoori.Scope
import Tandoori.CallGraph
import Control.Monad.State
import Language.Haskell.Syntax
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
    
tyCon :: PolyEnv -> HsQName -> Stateful HsType
tyCon p (UnQual name)                 = case getCon p name of
                                          Nothing -> do addError (UndefinedCon name)
                                                        createTv
                                          Just ty -> return ty
tyCon _ name | name == list_cons_name = do tv <- createTv
                                           return $ tyCurryFun [tv, tyList tv, tyList tv]


maptupM :: (Monad m) => (a -> m (b, c)) -> [a] -> m ([b], [c])
maptupM action items = do results <- mapM action items
                          return (map fst results, map snd results)

                                 
infer :: PolyEnv -> HsExp -> Stateful (MonoEnv, HsType)
infer p expr = withExpr expr (infer' p expr)

infer' :: PolyEnv -> HsExp -> Stateful (MonoEnv, HsType)
infer' p (HsLit lit) = return $ justType $ typeOfLit lit
infer' p (HsList exprs) = do (ms, ts) <- maptupM (infer p) exprs
                             (m, t) <- unify ms ts
                             return (m, tyList t)
infer' p (HsTuple exprs) = do (ms, ts) <- maptupM (infer p) exprs
                              return (combineMonos ms, tyTuple ts)
infer' p (HsCon name) = liftM justType (tyCon p name)
infer' p (HsApp fun param) = do (m1, ty1) <- infer p fun
                                (m2, ty2) <- infer p param
                                alpha <- createTv
                                (m, ty) <- unify [m1, m2] [ty1, HsTyFun ty2 alpha]
                                case ty of
                                  HsTyFun ty3 ty4 -> return (m, ty4)
                                  _ -> do beta <- createTv
                                          return (m, beta)
infer' p (HsVar (UnQual name)) | isLocal p name = newMonoVar
                               | otherwise      = case getPolyVar p name of
                                                    Nothing     -> do addError (UndefinedVar name)
                                                                      newMonoVar
                                                    Just (m, t) -> do t' <- instantiateTy isPoly t
                                                                      return (m, t')
                                                        where isPoly t = not (Set.member t monotyvars)
                                                              monotyvars = Set.unions $ map (tyVarsOf . snd) $ monoVars m
    where newMonoVar = do alpha <- createTv; return $ name `typedAs` alpha
infer' p (HsLambda srcloc pats expr) = withLoc srcloc $ do (ms, ts) <- maptupM (inferPat p') pats
                                                           let p'' = declareLocals p' (map fst $ concat $ map monoVars ms)
                                                           (m, t) <- infer p'' expr
                                                           alpha <- createTv
                                                           beta <- createTv
                                                           (m', t') <- unify (m:ms) [HsTyFun (tyCurryFun ts) alpha, HsTyFun beta t]
                                                           let m'' = removeMonoVars m' monovars
                                                           return (m'', t')
    where monovars = concat $ map boundNamesOfPatInt pats
          p' = removePolyVars p monovars               
infer' p (HsParen expr) = infer p expr
infer' p inf@(HsInfixApp _ _ _) = infer p (infixToPrefix inf)
infer' p (HsLet decls expr) = do let declss = sortDecls decls
                                 p' <- foldM inferDeclGroup p declss
                                 infer p' expr
                                       
inferDeclGroup :: PolyEnv -> [HsDecl] -> Stateful PolyEnv
inferDeclGroup p decls = do let newnamesInt = concat $ map boundNamesOfDeclInt decls
                                newnamesExt = concat $ map boundNamesOfDeclExt decls
                                p' = removePolyVars p newnamesInt
                            ms <- mapM (inferDef (declareLocals p' newnamesExt)) decls
                            (m, _) <- unify ms []
                            let m' = removeMonoVars m newnamesExt
                                definePoly p name = addPolyVar p name (reduce m' (fromJust $ m |->| name))
                                p'' = foldl definePoly p' newnamesExt
                            return p''
    where reduce :: MonoEnv -> HsType -> (MonoEnv, HsType)
          reduce m t = (m', t)
              where m' = let tyVars = tyVarsOf t
                         in filterMonoVars m (\ name ty -> Set.null $ (tyVarsOf ty) `Set.intersection` tyVars)

inferRhs p (HsUnGuardedRhs expr) = infer p expr
inferRhs p (HsGuardedRhss rhss) = do (ms, ts) <- maptupM inferGuardedRhs rhss
                                     unify ms ts
    where inferGuardedRhs (HsGuardedRhs srcloc expr guard) = withLoc srcloc $ do (m, t) <- infer p expr
                                                                                 (m', t') <- infer p guard
                                                                                 (m'', t'') <- unify [m'] [t', tyBool]
                                                                                 unify [m, m''] [t]
                                     
inferDef :: PolyEnv -> HsDecl -> Stateful MonoEnv
inferDef p (HsPatBind srcloc pat rhs wheres) = withLoc srcloc $ do (m, t) <- inferPat p pat
                                                                   let p' = declareLocals p (map fst $ monoVars m)
                                                                   (m', t') <- inferRhs p' rhs
                                                                   (m'', t'') <- unify [m, m'] [t, t']
                                                                   return m''
inferDef p (HsFunBind matches) = do ms <- mapM (inferMatch p) matches
                                    (m, _) <- unify ms []
                                    return m

inferMatch :: PolyEnv -> HsMatch -> Stateful MonoEnv
inferMatch p (HsMatch srcloc name pats rhs wheres) = withLoc srcloc $ do (ms, ts) <- maptupM (inferPat p) pats
                                                                         let p' = declareLocals p (map fst $ concat $ map monoVars ms)
                                                                         (m, t) <- inferRhs p' rhs
                                                                         alpha <- createTv
                                                                         beta <- createTv
                                                                         (m', t') <- unify (m:ms) [HsTyFun (tyCurryFun ts) alpha, HsTyFun beta t]
                                                                         return $ m' |+| (name, t')
                         
inferPat :: PolyEnv -> HsPat -> Stateful (MonoEnv, HsType)
inferPat p (HsPVar name) = do alpha <- createTv
                              return $ name `typedAs` alpha
inferPat p (HsPLit lit) = return $ justType $ typeOfLit lit
inferPat p (HsPApp conname pats) = do tycon <- tyCon p conname
                                      (ms, ts) <- maptupM (inferPat p) pats
                                      alpha <- createTv
                                      (m, t) <- unify ms [tycon, tyCurryFun (ts ++ [alpha])]
                                      return (m, last (tyUncurryFun t))
inferPat p (HsPTuple pats) = do (ms, ts) <- maptupM (inferPat p) pats
                                return (combineMonos ms, tyTuple ts)
inferPat p (HsPList pats) = do (ms, ts) <- maptupM (inferPat p) pats
                               unify ms ts                               
inferPat p (HsPAsPat name pat) = do (m, t) <- inferPat p pat
                                    return (m |+| (name, t), t)
inferPat p (HsPWildCard) = do alpha <- createTv
                              return $ justType alpha
inferPat p (HsPParen pat) = inferPat p pat
inferPat p (HsPInfixApp left conname right) = inferPat p (HsPApp conname [left, right])


                          
unify :: [MonoEnv] -> [HsType] -> Stateful (MonoEnv, HsType)
unify ms tys = do eqs <- monoeqs
                  alpha <- createTv
                  let eqs' = map (\ ty -> (alpha, ty)) tys
                  case mgu (eqs ++ eqs') of
                    Left errs -> do addError $ UnificationFailed ms errs
                                    return $ (combineMonos ms, alpha)
                    Right subst -> return $ (combineMonos (map (substMono subst) ms), substTy subst alpha)
                                  
    where monoeqs = do let vars = concat $ map monoVars ms
                           varnames = distinct $ map fst vars
                       tyvarmap <- liftM Map.fromList $ mapM (\ var -> do tv <- createTv; return (var, tv)) varnames
                       return $ map (\ (var, ty) -> (fromJust $ Map.lookup var tyvarmap, ty)) vars
                       -- return $ uncurry zip $ applyFst removeNothing $ unzip $ map (\ (var, ty) -> (Map.lookup var tyvarmap, ty)) vars
          distinct = Set.toList . Set.fromList
          removeNothing = map fromJust . filter isJust
          applyFst f (x, y) = (f x, y)                                                        
          substMono subst m = mapMono (substTy subst) m
