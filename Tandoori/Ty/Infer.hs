-- module Tandoori.Ty.Infer(inferValBinds) where
module Tandoori.Ty.Infer where

import Tandoori
import Tandoori.Util
import Tandoori.State    
import Tandoori.Errors
import Tandoori.Ty
import Tandoori.Ty.Canonize
import Tandoori.Ty.MonoEnv
import Tandoori.Ty.Ctxt
import Tandoori.Ty.Unify
import Tandoori.Ty.Instantiate
import Control.Monad.State
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

import Tandoori.GHC.Internals
    
import Bag (bagToList)
    
tyCon :: Ctxt -> ConName -> Stateful TanType
tyCon c name  | name == dataConName nilDataCon     = do alpha <- mkTv
                                                        return $ tyList alpha
                                                          
tyCon c name  | name == dataConName consDataCon    = do alpha <- mkTv
                                                        return $ tyCurryFun [alpha, tyList alpha, tyList alpha]

-- -- TODO: Clean up builtin constructors mess
-- tyCon c name  | name == dataConName trueDataCon ||
--                 name == dataConName falseDataCon   = return tyBool             
tyCon c name                                       = case getCon c name of
                                                       Nothing -> do addError (UndefinedCon name)
                                                                     mkTv
                                                       Just ty -> instantiateTy ty
                             
maptupM :: (Monad m) => (a -> m (b, c)) -> [a] -> m ([b], [c])
maptupM action items = do results <- mapM action items
                          return (map fst results, map snd results)
                                 
maptup3M :: (Monad m) => (a -> m (b, c, d)) -> [a] -> m ([b], [c], [d])
maptup3M action items = do results <- mapM action items
                           return (map fst3 results, map snd3 results, map trd3 results)
    where fst3 (x, y, z) = x
          snd3 (x, y, z) = y
          trd3 (x, y, z) = z

doLoc :: SrcSpan -> Stateful a -> Stateful a
doLoc srcloc m | isGoodSrcSpan srcloc  = withLoc srcloc $ m
               | otherwise             = m
           
infer :: Ctxt -> (Located TanExpr) -> Stateful (MonoEnv, TanType)
infer c (L srcloc expr) = doLoc srcloc $ withSrc expr $ infer' c expr

typeOfOverLit :: HsOverLit Name -> Stateful TanType
typeOfOverLit (OverLit { ol_val = HsIsString _ }) = return tyString
typeOfOverLit (OverLit { ol_val = val }) = do alpha@(HsTyVar alpha_name) <- mkTv
                                              let bndrs = [genLoc $ UserTyVar alpha_name]
                                                  tc = case val of
                                                         HsIntegral _ -> numClassName
                                                  preds = [genLoc $ HsClassP tc [genLoc alpha]]
                                                  lpreds = genLoc preds
                                              return $ HsForAllTy Implicit bndrs lpreds (genLoc alpha)
                                                             
infer' :: Ctxt -> TanExpr -> Stateful (MonoEnv, TanType)
infer' c (HsLit lit)                       = return $ justType $ typeOfLit lit
infer' c (HsOverLit overlit)               = liftM justType $ typeOfOverLit overlit
infer' c (HsVar name) | isDataConName name = do ty <- tyCon c name
                                                ty' <- canonizeTy c ty
                                                return $ justType ty'
                                             
infer' c (ExplicitList _ lexprs)           = do (ms, ts) <- maptupM (infer c) lexprs
                                                (m, t) <- unify c ms ts
                                                return (m, tyList t)
infer' c (ExplicitTuple lexprs _)          = do (ms, ts) <- maptupM (infer c) lexprs
                                                unify c ms [tyTuple ts]
                                                       
infer' c (HsApp lfun lparam)               = do (m1, ty1) <- infer c lfun
                                                (m2, ty2) <- infer c lparam
                                                alpha <- mkTv
                                                (m, ty) <- unify c [m1, m2] [ty1, HsFunTy (noLoc ty2) (noLoc alpha)]
                                                ty' <- canonizeTy c ty
                                                case ty' of
                                                  HsFunTy (L _ ty3) (L _ ty4)                    -> return (m, ty4)
                                                  HsForAllTy e _ lctxt (L _ (HsFunTy lty3 lty4)) -> return (m, (HsForAllTy e noBinder lctxt lty4))
                                                  _                                              -> do beta <- mkTv
                                                                                                       return (m, beta)
                                                                 
infer' c (HsLam (MatchGroup lmatches _))   = do (ms, ts) <- maptupM (inferMatch c) matches
                                                unify c ms ts
    where matches = map unLoc lmatches

infer' c (HsVar name) = case getUserDecl c name of
                          Just lty  -> do ty' <- instantiateTy (unLoc lty)
                                          ty'' <- canonizeTy c ty'
                                          return $ justType ty''
                          Nothing   -> case getPolyVar c name of
                                         Nothing -> do alpha <- mkTv
                                                       return $ name `typedAs` alpha
                                         Just (m, t) -> do t' <- instantiateTy t
                                                           return (m, t')
                              
infer' c (HsLet binds lexpr)               = do c' <- inferLocalBinds c binds
                                                infer c' lexpr

infer' c (OpApp left op fixity right)      = infer' c $ HsApp (genLoc $ HsApp op left) right
-- TODO:
infer' c (NegApp expr negfun)              = error "infer': TODO: NegApp"
infer' c (HsPar lexpr)                     = infer c lexpr

-- inferRhs c (HsUnGuardedRhs expr) = infer c expr
-- inferRhs c (GRHS _ lexpr) = do (ms, ts) <- maptupM infer lexprs
--                                unify c ms ts
--     where inferGuardedRhs (HsGuardedRhs srcloc expr guard) = withLoc srcloc $ do (m, t) <- infer c expr
--                                                                                  (m', t') <- infer c guard
--                                                                                  (m'', t'') <- unify c [m'] [t', tyBool]
--                                                                                  unify c [m, m''] [t]

inferGRhs c (GRHS _ lexpr) = infer c lexpr

inferGRhss c (GRHSs lgrhss _) = do (ms, ts) <- maptupM (inferGRhs c . unLoc) lgrhss
                                   unify c ms ts
                             
inferLocalBinds :: Ctxt -> HsLocalBinds Name -> Stateful Ctxt
inferLocalBinds c (HsValBinds vb)     = inferValBinds c vb
inferLocalBinds c (HsIPBinds ipbinds) = error "inferLocalBinds: HsIPBnds"
inferLocalBinds c EmptyLocalBinds     = return c

inferValBinds :: Ctxt -> HsValBinds Name -> Stateful Ctxt
inferValBinds c (ValBindsOut recbinds lsigs) = foldM inferBinds c' (map snd recbinds)
    where c' = addUserDecls c lsigs
                                               
inferBinds :: Ctxt -> LHsBinds Name -> Stateful Ctxt
inferBinds c lbinds = do let binds = map unLoc $ bagToList lbinds
                         (nss, ms) <- maptupM (inferBind c) binds
                         let ns = Set.unions nss
                         (m, _) <- unify c ms []
                         foldM (definePoly m) c (Set.toList ns)
                               
    where definePoly m c name = case (getUserDecl c name) of
                                  Nothing              -> do ty' <- canonizeTy c ty
                                                             return $ addPolyVar c name (m, ty')
                                  Just (L loc tyDecl)  -> doLoc loc $
                                                          do ty' <- canonizeTy c ty
                                                             case fitDecl tyDecl ty' of
                                                               Left errs         -> do addError $ CantFitDecl tyDecl ty' errs
                                                                                       return $ c
                                                               Right (s, preds)  -> if ensuresPredicates preds tyDecl'
                                                                                    then return c
                                                                                    else do addError $ CantFitDecl tyDecl ty' []
                                                                                            return $ c
                                                                   where tyDecl' = substTy s tyDecl
              where Just ty = getMonoVar m name
                                                                                               
inferBind :: Ctxt -> (HsBind Name) -> Stateful (Set.Set Name, MonoEnv)
inferBind c PatBind{pat_lhs = lpat, pat_rhs = grhss} = do (ns, m, t) <- inferPat c (unLoc lpat)
                                                          (m', t') <- inferGRhss c grhss
                                                          (m'', _) <- unify c [m, m'] [t, t']
                                                          return (ns, m'')
inferBind c VarBind{} = error "VarBind"
inferBind c FunBind{fun_matches = MatchGroup lmatches _, fun_id = (L _ name)} = do (ms, ts) <- maptupM inferLMatch lmatches
                                                                                   (m, t) <- unify c ms ts
                                                                                   let m' = addMonoVar m (name, t)
                                                                                   return (Set.singleton name, m')
    where inferLMatch (L srcloc match) = doLoc srcloc $ inferMatch c match
               
inferMatch :: Ctxt -> (Match Name) -> Stateful (MonoEnv, TanType)
inferMatch c (Match lpats _ grhss) = do (nss, ms, ts) <- maptup3M (inferPat c . unLoc) lpats
                                        let ns = Set.unions nss
                                        (m, t) <- inferGRhss c grhss
                                        (m', t') <- unify c (m:ms) [tyCurryFun (ts ++ [t])]
                                        let m'' = filterMonoVars (\ n ty -> not (Set.member n ns)) m'
                                        return $ (m'', t')

justTypePat :: TanType -> (Set.Set VarName, MonoEnv, TanType)
justTypePat ty = let (m, t) = justType ty in (Set.empty, m, t)

typedAsPat :: VarName -> TanType -> (Set.Set VarName, MonoEnv, TanType)
name `typedAsPat` ty = let (m, t) = name `typedAs` ty in (Set.singleton name, m, t)
                                               
inferPat :: Ctxt -> TanPat -> Stateful (Set.Set VarName, MonoEnv, TanType)
inferPat c (AsPat (L _ name) (L _ pat))      = do (ns, m, t) <- inferPat c pat
                                                  let m' = addMonoVar m (name, t)
                                                  return (ns, m', t)
inferPat c (ParPat (L _ pat))                = inferPat c pat
inferPat c (WildPat _)                       = do alpha <- mkTv
                                                  return $ justTypePat alpha
inferPat c (VarPat name)                     = do alpha <- mkTv
                                                  return $ name `typedAsPat` alpha
inferPat c (LitPat lit)                      = return $ justTypePat $ typeOfLit lit
inferPat c (ConPatIn (L _ conname) details)  = do tycon <- tyCon c conname
                                                  (nss, ms, ts) <- maptup3M (inferPat c) pats
                                                  let ns = Set.unions nss
                                                  alpha <- mkTv
                                                  (m, t) <- unify c ms [tycon, tyCurryFun (ts ++ [alpha])]
                                                  return (ns, m, last (tyUncurryFun t))
    where pats = case details of
                   PrefixCon lpats            -> map unLoc lpats
                   InfixCon (L _ p) (L _ p')  -> [p, p']
inferPat c (TuplePat lpats _ _)              = do (nss, ms, ts) <- maptup3M (inferPat c . unLoc) lpats
                                                  return (Set.unions nss, combineMonos ms, tyTuple ts)
inferPat c (ListPat lpats _)                 = do (nss, ms, ts) <- maptup3M (inferPat c . unLoc) lpats
                                                  (m', t') <- unify c ms ts
                                                  return (Set.unions nss, m', tyList t')
inferPat c (NPat overlit _ _)                = liftM justTypePat $ typeOfOverLit overlit

                          
unify :: Ctxt -> [MonoEnv] -> [TanType] -> Stateful (MonoEnv, TanType)
unify c ms tys = do eqs <- monoeqs
                    alpha <- mkTv
                    let eqs' = map (\ ty -> (alpha, ty)) tys
                    case mgu (eqs ++ eqs') of
                      Left errs -> do addError $ UnificationFailed ms errs
                                      return $ (combineMonos ms, alpha)
                      Right r -> do ms' <- mapM (xformMono r) ms
                                    ty' <- xformTy r alpha
                                    return $ (combineMonos ms', ty')
                                  
    where monoeqs = do let vars = concat $ map monoVars ms
                           varnames = distinct $ map fst vars
                       tyvarmap <- liftM Map.fromList $ mapM (\ var -> do tv <- mkTv; return (var, tv)) varnames
                       return $ map (\ (var, ty) -> (fromJust $ Map.lookup var tyvarmap, ty)) vars
          distinct = Set.toList . Set.fromList
                     
          xformMono r m = mapMonoM (xformTy r) m
          xformTy (s, preds) ty = do let ty' = substTy s ty                                                           
                                         ty'' = HsForAllTy Implicit noBinder lctxt lty'
                                             where ctxt = map noLoc preds
                                                   lctxt = noLoc ctxt
                                                   lty' = noLoc ty'
                                     canonizeTy c ty''
