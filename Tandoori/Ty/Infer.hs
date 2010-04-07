-- module Tandoori.Ty.Infer(inferValBinds) where
module Tandoori.Ty.Infer where

import Tandoori
import Tandoori.Util
import Tandoori.State    
import Tandoori.Errors
import Tandoori.Ty
import Tandoori.Ty.Canonize
import Tandoori.Ty.Predicates
import Tandoori.Ty.MonoEnv
import Tandoori.Ty.Ctxt
import Tandoori.Ty.Unify
import Tandoori.Ty.Instantiate
import Control.Monad.State
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

import Tandoori.GHC.Internals

import Module    
    
import Bag (bagToList)

mkCTv = liftM noPreds mkTv
    
tyCon :: Ctxt -> ConName -> Stateful CanonizedType
tyCon c name  | name == dataConName nilDataCon     = do alpha <- mkCTv
                                                        return $ tyList alpha
                                                          
tyCon c name  | name == dataConName consDataCon    = do alpha <- mkCTv
                                                        return $ tyCurryFun [alpha, tyList alpha, tyList alpha]

--TODO: Clean up builtin constructors mess
tyCon c name  | name == dataConName trueDataCon ||
                name == dataConName falseDataCon   = return tyBool
tyCon c name                                       = case getCon c name of
                                                       Nothing -> do addError (UndefinedCon name)
                                                                     mkCTv
                                                       Just cty -> instantiateTy (const True) cty
                             
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
           
infer :: Ctxt -> (Located TanExpr) -> Stateful (MonoEnv, CanonizedType)
infer c (L srcloc expr) = doLoc srcloc $ withSrc expr $ infer' c expr

typeOfOverLit :: HsOverLit Name -> Stateful CanonizedType
typeOfOverLit (OverLit { ol_val = HsIsString _ }) = return $ tyString
typeOfOverLit (OverLit { ol_val = val }) = do alpha <- mkTv
                                              let tc = case val of
                                                         HsIntegral _ -> numClassName
                                                  lpred = genLoc $ HsClassP tc [genLoc alpha]
                                              return $ mkCanonizedType alpha [lpred]
                                              return $ tyInt -- Uncomment this to have non-polymorph integer literals
                                                             
infer' :: Ctxt -> TanExpr -> Stateful (MonoEnv, CanonizedType)
infer' c (HsLit lit)                       = return $ justType $ typeOfLit lit
infer' c (HsOverLit overlit)               = liftM justType $ typeOfOverLit overlit
infer' c (HsVar name) | isDataConName name = do ty <- tyCon c name
                                                return $ justType ty
                                             
infer' c (ExplicitList _ lexprs)           = do (ms, ts) <- maptupM (infer c) lexprs
                                                (m, t) <- unify c ms ts
                                                return (m, tyList t)
infer' c (ExplicitTuple lexprs _)          = do (ms, ts) <- maptupM (infer c) lexprs
                                                unify c ms [tyTuple ts]
                                                       
infer' c (HsApp ltyFun ltyParam)           = do (m1, ty1) <- infer c ltyFun
                                                (m2, ty2) <- infer c ltyParam
                                                alpha <- mkCTv
                                                (m, ty) <- unify c [m1, m2] [ty1, tyCurryFun [ty2, alpha]]
                                                case ctyTy ty of
                                                  HsFunTy (L _ ty3) (L _ ty4) -> return (m, mkCanonizedType ty4 (ctyLPreds ty))
                                                  _                           -> do beta <- mkCTv
                                                                                    return (m, beta)
                                                                 
infer' c (HsLam (MatchGroup lmatches _))   = do (ms, ts) <- maptupM (inferMatch c) matches
                                                unify c ms ts
    where matches = map unLoc lmatches

infer' c (HsVar name) = case getUserDecl c name of
                          Just lty  -> do ty' <- instantiateTy (const True) (unLoc lty)
                                          return $ justType ty'
                          Nothing   -> case getPolyVar c name of
                                         Nothing -> do alpha <- mkCTv
                                                       return $ name `typedAs` alpha
                                         Just (m, t) -> do t' <- instantiateTy isPoly t
                                                           return (m, t')
                                             where isPoly tv = not (Set.member tv monotyvars)
                                                   monotyvars = Set.unions $ map tyVarsOfDef $ Set.toList $ forcedMonoVars c
                                                   tyVarsOfDef n = case getMonoVar m n of
                                                                     Nothing -> Set.empty
                                                                     Just ty -> tyVarsOf $ ctyTy ty

                              
infer' c (HsLet binds lexpr)               = do (ns, c', m') <- inferLocalBinds c binds
                                                (m, t) <- infer c' lexpr
                                                (m'', t'') <- unify c [m, m'] [t]
                                                let isOutsideVisible n t = not(n `Set.member` ns)
                                                    mOut = filterMonoVars isOutsideVisible m''
                                                return (mOut, t'')

infer' c (OpApp left op fixity right)      = infer' c $ HsApp (genLoc $ HsApp op left) right
-- TODO:
infer' c (NegApp expr negfun)              = error "infer': TODO: NegApp"
infer' c (HsPar lexpr)                     = infer c lexpr

--     where inferGuardedRhs (HsGuardedRhs srcloc expr guard) = withLoc srcloc $ do (m, t) <- infer c expr
--                                                                                  (m', t') <- infer c guard
--                                                                                  (m'', _) <- unify c [m'] [t', tyBool]
--                                                                                  unify c [m, m''] [t]

inferGRhs c (GRHS _ lexpr) = infer c lexpr

inferGRhss c (GRHSs lgrhss _) = do (ms, ts) <- maptupM (inferGRhs c . unLoc) lgrhss
                                   unify c ms ts
                             
inferLocalBinds :: Ctxt -> HsLocalBinds Name -> Stateful (Set.Set Name, Ctxt, MonoEnv)
inferLocalBinds c (HsValBinds vb)     = inferValBinds c vb
inferLocalBinds c (HsIPBinds ipbinds) = error "inferLocalBinds: HsIPBnds"
inferLocalBinds c EmptyLocalBinds     = return (Set.empty, c, emptyMono)

inferValBinds :: Ctxt -> HsValBinds Name -> Stateful (Set.Set Name, Ctxt, MonoEnv)
inferValBinds c (ValBindsOut recbinds lsigs) = foldM foldBinds (Set.empty, c', emptyMono) (map snd recbinds)
    where c' = addUserDecls c lsigs
          foldBinds (ns, c, m) binds = do (ns', c', m') <- inferBinds c binds
                                          (m'', _) <- unify c [m, m'] []
                                          return (ns `Set.union` ns', c', m'')
                                               
inferBinds :: Ctxt -> LHsBinds Name -> Stateful (Set.Set Name, Ctxt, MonoEnv)
inferBinds c lbindbag = do let lbinds = bagToList lbindbag
                           (nss, ms) <- maptupM (inferLBind c) lbinds
                           let ns = Set.unions nss
                           (m, _) <- unify c ms []
                           let m' = filterMonoVars (\ n t -> not(n `Set.member` ns)) m -- Kill?
                           c' <- foldM (definePoly m) c $ map (\ n -> (n, fromJust $ getMonoVar m n)) (Set.toList ns)
                           return (ns, c', m)
                               
    where definePoly m c (name, ty) = case (getUserDecl c name) of
                                        Nothing              -> do ty' <- resolvePreds c ty
                                                                   return $ addPolyVar c name (m, ty')                                        
                                        Just (L loc tyDecl)  -> doLoc loc $ do
                                                                  case fitDecl (ctyTy tyDecl) (ctyTy ty) of
                                                                    Left errs         -> do addError $ CantFitDecl tyDecl ty errs
                                                                                            return $ c
                                                                    Right s  -> do ty' <- resolvePreds c (substCTy s (ty, []))
                                                                                   let lpreds = ctyLPreds ty'
                                                                                   ensuresPreds <- ensuresPredicates c lpreds tyDecl'
                                                                                   if ensuresPreds
                                                                                     then return c
                                                                                     else do addError $ CantFitDecl tyDecl ty' []
                                                                                             return $ c
                                                                        where tyDecl' = substCTy s (tyDecl, [])

inferLBind :: Ctxt -> (LHsBind Name) -> Stateful (Set.Set Name, MonoEnv)
inferLBind c lbind = withLSrc lbind $ inferBind c $ unLoc lbind
                                                                                        
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
               
inferMatch :: Ctxt -> (Match Name) -> Stateful (MonoEnv, CanonizedType)
inferMatch c (Match lpats _ grhss) = do (nss, ms, ts) <- maptup3M (inferPat c . unLoc) lpats
                                        let ns = Set.unions nss
                                            c' = forceMonoVars c ns
                                        (m, t) <- inferGRhss c' grhss
                                        (m', t') <- unify c (m:ms) [tyCurryFun (ts ++ [t])]
                                        let m'' = filterMonoVars (\ n ty -> not (Set.member n ns)) m'
                                        return $ (m'', t')

justTypePat :: CanonizedType -> (Set.Set VarName, MonoEnv, CanonizedType)
justTypePat ty = let (m, t) = justType ty in (Set.empty, m, t)

typedAsPat :: VarName -> CanonizedType -> (Set.Set VarName, MonoEnv, CanonizedType)
name `typedAsPat` cty = let (m, t) = name `typedAs` cty in (Set.singleton name, m, t)
                                               
inferPat :: Ctxt -> TanPat -> Stateful (Set.Set VarName, MonoEnv, CanonizedType)
inferPat c (AsPat (L _ name) (L _ pat))      = do (ns, m, t) <- inferPat c pat
                                                  let m' = addMonoVar m (name, t)
                                                  return (ns, m', t)
inferPat c (ParPat (L _ pat))                = inferPat c pat
inferPat c (WildPat _)                       = do alpha <- mkTv
                                                  return $ justTypePat $ noPreds alpha
inferPat c (VarPat name)                     = do alpha <- mkTv
                                                  return $ name `typedAsPat` (noPreds alpha)
inferPat c (LitPat lit)                      = return $ justTypePat $ typeOfLit lit
inferPat c (ConPatIn (L _ conname) details)  = do tycon <- tyCon c conname
                                                  (nss, ms, ts) <- maptup3M (inferPat c) pats
                                                  let ns = Set.unions nss
                                                  alpha <- mkCTv
                                                  (m, t) <- unify c ms [tycon, tyCurryFun (ts ++ [alpha])]
                                                  return $ (ns, m, tyFunResult t)
    where pats = case details of
                   PrefixCon lpats            -> map unLoc lpats
                   InfixCon (L _ p) (L _ p')  -> [p, p']
          tyFunResult cty = mkCanonizedType tyResult (ctyLPreds cty)
              where tyResult = tyRightmost $ ctyTy cty
          tyRightmost (HsFunTy (L _ tyLeft) (L _ tyRight)) = tyRightmost tyRight
          tyRightmost ty                                   = ty
                                                 
inferPat c (TuplePat lpats _ _)              = do (nss, ms, ts) <- maptup3M (inferPat c . unLoc) lpats
                                                  return (Set.unions nss, combineMonos ms, tyTuple ts)
inferPat c (ListPat lpats _)                 = do (nss, ms, ts) <- maptup3M (inferPat c . unLoc) lpats
                                                  (m', t') <- unify c ms ts
                                                  return (Set.unions nss, m', tyList t')
inferPat c (NPat overlit _ _)                = liftM justTypePat $ typeOfOverLit overlit

                          
unify :: Ctxt -> [MonoEnv] -> [CanonizedType] -> Stateful (MonoEnv, CanonizedType)
unify c ms tys = do eqs <- monoeqs
                    alpha <- mkTv
                    let eqs' = map (\ ty -> (alpha, ctyTy ty)) tys
                        calpha = mkCanonizedType alpha (concatMap ctyLPreds tys)
                    case mgu eqs eqs' of
                      Left errs -> do addError $ UnificationFailed ms errs
                                      return $ (combineMonos ms, noPreds alpha)
                      Right s -> do let ms' = map (xformMono s) ms
                                        ty' = xformTy s calpha
                                    return $ (combineMonos ms', ty')
                                  
    where monoeqs = do tyvarmap <- liftM Map.fromList $ mapM (\ var -> do tv <- mkTv; return (var, tv)) varnames
                       return $ map (\ (var, ty) -> (fromJust $ Map.lookup var tyvarmap, ctyTy ty)) vars
              where varnames = distinct $ map fst vars
                    distinct = Set.toList . Set.fromList
                               
          vars = concat $ map monoVars ms
          monolpreds = concatMap (ctyLPreds . snd) vars
                     
          xformMono s m = mapMono (xformTy s) m
          xformTy s ty = substCTy s (ty, monolpreds)
