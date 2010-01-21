module Tandoori.Ty.Infer(inferValBinds) where

import Tandoori
import Tandoori.State    
import Tandoori.Errors
import Tandoori.Ty
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
tyCon c name | name == dataConName nilDataCon  = do alpha <- mkTv
                                                    return $ tyList alpha
                                                          
tyCon c name | name == dataConName consDataCon = do alpha <- mkTv
                                                    return $ tyCurryFun [alpha, tyList alpha, tyList alpha]
                                                          
tyCon c name                                   = case getCon c name of
                                                   Nothing -> do addError (UndefinedCon name)
                                                                 mkTv
                                                   Just ty -> instantiateTy (const True) ty
                             
maptupM :: (Monad m) => (a -> m (b, c)) -> [a] -> m ([b], [c])
maptupM action items = do results <- mapM action items
                          return (map fst results, map snd results)

genLoc x = mkGeneralLocated "(internal)" x

doLoc :: SrcSpan -> Stateful a -> Stateful a
doLoc srcloc m | isGoodSrcSpan srcloc  = withLoc srcloc $ m
               | otherwise             = m
           
infer :: Ctxt -> (Located TanExpr) -> Stateful (MonoEnv, TanType)
infer c (L srcloc expr) = doLoc srcloc $ withSrc expr $ infer' c expr

infer' :: Ctxt -> TanExpr -> Stateful (MonoEnv, TanType)
infer' c (HsLit lit)                       = return $ justType $ typeOfLit lit
infer' c (HsOverLit overlit)               = return $ justType $ typeOfOverLit overlit
infer' c (HsVar name) | isDataConName name = liftM justType (tyCon c name)
                                             
infer' c (ExplicitList _ lexprs)           = do (ms, ts) <- maptupM (infer c) lexprs
                                                (m, t) <- unify ms ts
                                                return (m, tyList t)
infer' c (ExplicitTuple lexprs _)          = do (ms, ts) <- maptupM (infer c) lexprs
                                                return (combineMonos ms, tyTuple ts)
                                                       
infer' c (HsApp lfun lparam)               = do (m1, ty1) <- infer c lfun
                                                (m2, ty2) <- infer c lparam
                                                alpha <- mkTv
                                                (m, ty) <- unify [m1, m2] [ty1, HsFunTy (genLoc ty2) (genLoc alpha)]
                                                case ty of
                                                  HsFunTy (L _ ty3) (L _ ty4) -> return (m, ty4)
                                                  _                           -> do beta <- mkTv
                                                                                    return (m, beta)
                                                                 
infer' c (HsLam (MatchGroup lmatches _))   = do (ms, ts) <- maptupM (inferMatch c) matches
                                                unify ms ts
    where matches = map unLoc lmatches

infer' c (HsVar name) = case getUserDecl c name of
                          Just lty  -> do ty' <- instantiateTy (const True) (unLoc lty)
                                          return $ justType ty'
                          Nothing   -> inferVar
                                     
    where inferVar | isLocal c name = newMonoVar
                   | otherwise      = case getPolyVar c name of
                                        Nothing     -> do addError (UndefinedVar name)
                                                          newMonoVar
                                        Just (m, t) -> do t' <- instantiateTy isPoly t
                                                          return (m, t')
                                            where isPoly t = not (Set.member t monotyvars)
                                                  monotyvars = Set.unions $ map (tyVarsOf . snd) $ monoVars m
              where newMonoVar = do alpha <- mkTv
                                    return $ name `typedAs` alpha
                              
infer' c (HsLet binds lexpr)               = do c' <- inferLocalBinds c binds
                                                infer c' lexpr

infer' c (OpApp left op fixity right)      = infer' c $ HsApp (genLoc $ HsApp op left) right
-- TODO:
infer' c (NegApp expr negfun)              = error "infer': TODO: NegApp"
infer' c (HsPar lexpr)                     = infer c lexpr

-- inferRhs c (HsUnGuardedRhs expr) = infer c expr
-- inferRhs c (GRHS _ lexpr) = do (ms, ts) <- maptupM infer lexprs
--                                unify ms ts
--     where inferGuardedRhs (HsGuardedRhs srcloc expr guard) = withLoc srcloc $ do (m, t) <- infer c expr
--                                                                                  (m', t') <- infer c guard
--                                                                                  (m'', t'') <- unify [m'] [t', tyBool]
--                                                                                  unify [m, m''] [t]

inferGRhs c (GRHS _ lexpr) = infer c lexpr

inferGRhss c (GRHSs lgrhss _) = do (ms, ts) <- maptupM (inferGRhs c . unLoc) lgrhss
                                   unify ms ts
                             
inferLocalBinds :: Ctxt -> HsLocalBinds Name -> Stateful Ctxt
inferLocalBinds c (HsValBinds vb)     = inferValBinds c vb
inferLocalBinds c (HsIPBinds ipbinds) = error "inferLocalBinds: HsIPBnds"
inferLocalBinds c EmptyLocalBinds     = return c

inferValBinds :: Ctxt -> HsValBinds Name -> Stateful Ctxt
inferValBinds c (ValBindsOut recbinds lsigs) = foldM inferBinds c' (map snd recbinds)
    where c' = addUserDecls c lsigs
                                               
inferBinds :: Ctxt -> LHsBinds Name -> Stateful Ctxt
inferBinds c lbinds = do (ms, ts) <- maptupM (inferBind c') binds
                         foldM definePoly c $ zip3 boundnames ms ts
                               
    where binds = map unLoc $ bagToList lbinds
          c' = declareLocals c boundnames
          boundnames = map boundname binds
              where boundname FunBind{fun_id = (L _ name)} = name
                    boundname VarBind{var_id = name}       = name
          definePoly c (name, m, t) = case (getUserDecl c name) of
                                        Nothing              -> return $ addPolyVar c name (m, t)
                                        Just (L loc tyDecl)  -> doLoc loc $
                                                                case fitDecl tyDecl t of
                                                                  Left errs  -> do addError $ CantFitDecl tyDecl t errs
                                                                                   return $ c
                                                                  Right _    -> return $ c
                                                   
inferBind :: Ctxt -> (HsBind Name) -> Stateful (MonoEnv, TanType)
inferBind c FunBind{fun_matches = MatchGroup lmatches _} = do (ms, ts) <- maptupM inferLMatch lmatches
                                                              unify ms ts
    where inferLMatch (L srcloc match) = doLoc srcloc $ inferMatch c match
               
inferMatch :: Ctxt -> (Match Name) -> Stateful (MonoEnv, TanType)
inferMatch c (Match lpats _ grhss) = do (ms, ts) <- maptupM (inferPat c . unLoc) lpats
                                        let patternvars = map fst $ concat $ map monoVars ms                                                          
                                            c' = declareLocals (newScope c) $ patternvars
                                        (m, t) <- inferGRhss c' grhss
                                        (m', t') <- unify (m:ms) [tyCurryFun (ts ++ [t])]
                                        return $ (restrictScope c m', t')
          -- inferScope c f = do let c' = newScope c
          --                     (m, t) <- f c
          --                     return $ (restrictScope c m, t)

inferPat :: Ctxt -> TanPat -> Stateful (MonoEnv, TanType)
inferPat c (AsPat (L _ name) (L _ pat))      = do (m, t) <- inferPat c pat
                                                  return (addMonoVar m (name, t), t)
inferPat c (ParPat (L _ pat))                = inferPat c pat
inferPat c (WildPat _)                       = do alpha <- mkTv
                                                  return $ justType alpha
inferPat c (VarPat name)                     = do alpha <- mkTv
                                                  return $ name `typedAs` alpha
inferPat c (LitPat lit)                      = return $ justType $ typeOfLit lit
inferPat c (ConPatIn (L _ conname) details)  = do tycon <- tyCon c conname
                                                  (ms, ts) <- maptupM (inferPat c) pats
                                                  alpha <- mkTv
                                                  (m, t) <- unify ms [tycon, tyCurryFun (ts ++ [alpha])]
                                                  return (m, last (tyUncurryFun t))
    where pats = case details of
                   PrefixCon lpats            -> map unLoc lpats
                   InfixCon (L _ p) (L _ p')  -> [p, p']
inferPat c (TuplePat lpats _ _)              = do (ms, ts) <- maptupM (inferPat c . unLoc) lpats
                                                  return (combineMonos ms, tyTuple ts)
inferPat c (ListPat lpats _)                 = do (ms, ts) <- maptupM (inferPat c . unLoc) lpats
                                                  (m', t') <- unify ms ts
                                                  return (m', tyList t')
inferPat c (NPat overlit _ _)                = return $ justType $ typeOfOverLit overlit

                          
unify :: [MonoEnv] -> [TanType] -> Stateful (MonoEnv, TanType)
unify ms tys = do eqs <- monoeqs
                  alpha <- mkTv
                  let eqs' = map (\ ty -> (alpha, ty)) tys
                  case mgu (eqs ++ eqs') of
                    Left errs -> do addError $ UnificationFailed ms errs
                                    return $ (combineMonos ms, alpha)
                    Right subst -> return $ (combineMonos (map (substMono subst) ms), substTy subst alpha)
                                  
    where monoeqs = do let vars = concat $ map monoVars ms
                           varnames = distinct $ map fst vars
                       tyvarmap <- liftM Map.fromList $ mapM (\ var -> do tv <- mkTv; return (var, tv)) varnames
                       return $ map (\ (var, ty) -> (fromJust $ Map.lookup var tyvarmap, ty)) vars
          distinct = Set.toList . Set.fromList
          substMono subst m = mapMono (substTy subst) m
