module Tandoori.Ty.Infer where

import Tandoori
import Tandoori.State    
import Tandoori.Errors
import Tandoori.Ty
import Tandoori.Ty.MonoEnv
import Tandoori.Ty.PolyEnv
import Tandoori.Ty.Unify
import Tandoori.Ty.Instantiate
import Control.Monad.State
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
    
import HsExpr
import HsTypes
import HsPat
import SrcLoc
import HsLit
import Name (Name, isDataConName)
import HsBinds
import Bag (bagToList)
import TysWiredIn (listTyConName, nilDataCon, consDataCon)
import DataCon (dataConName)
    
tyCon :: PolyEnv -> ConName -> Stateful TanType
tyCon p name | name == dataConName nilDataCon  = do alpha <- mkTv
                                                    return $ tyList alpha
                                                          
tyCon p name | name == dataConName consDataCon = do alpha <- mkTv
                                                    return $ tyCurryFun [alpha, tyList alpha, tyList alpha]
                                                          
tyCon p name                                   = case getCon p name of
                                                   Nothing -> do addError (UndefinedCon name)
                                                                 mkTv
                                                   Just ty -> instantiateTy (const True) ty
                             
-- tyCon _ name | name == list_cons_name = do tv <- createTv
--                                            return $ tyCurryFun [tv, tyList tv, tyList tv]


maptupM :: (Monad m) => (a -> m (b, c)) -> [a] -> m ([b], [c])
maptupM action items = do results <- mapM action items
                          return (map fst results, map snd results)

genLoc x = mkGeneralLocated "(internal)" x
                                 
infer :: PolyEnv -> (Located TanExpr) -> Stateful (MonoEnv, TanType)
-- infer p expr = withExpr expr (infer' p expr)
infer p (L srcloc expr) | isGoodSrcSpan srcloc = withLoc srcloc $ infer' p expr
                        | otherwise            = infer' p expr

infer' :: PolyEnv -> TanExpr -> Stateful (MonoEnv, TanType)
infer' p (HsLit lit)                       = return $ justType $ typeOfLit lit
infer' p (HsOverLit overlit)               = return $ justType $ typeOfOverLit overlit
infer' p (HsVar name) | isDataConName name = liftM justType (tyCon p name)
                                             
infer' p (ExplicitList _ lexprs)           = do (ms, ts) <- maptupM (infer p) lexprs
                                                (m, t) <- unify ms ts
                                                return (m, tyList t)
infer' p (ExplicitTuple lexprs _)          = do (ms, ts) <- maptupM (infer p) lexprs
                                                return (combineMonos ms, tyTuple ts)
                                                       
infer' p (HsApp lfun lparam)               = do (m1, ty1) <- infer p lfun
                                                (m2, ty2) <- infer p lparam
                                                alpha <- mkTv
                                                (m, ty) <- unify [m1, m2] [ty1, HsFunTy (genLoc ty2) (genLoc alpha)]
                                                case ty of
                                                  HsFunTy (L _ ty3) (L _ ty4) -> return (m, ty4)
                                                  _                           -> do beta <- mkTv
                                                                                    return (m, beta)
                                                                 
infer' p (HsLam (MatchGroup lmatches _))   = do (ms, ts) <- maptupM (inferMatch p) matches
                                                unify ms ts
    where matches = map unLoc lmatches
                
infer' p (HsVar name) | isLocal p name     = newMonoVar
                      | otherwise          = case getPolyVar p name of
                                               Nothing     -> do addError (UndefinedVar name)
                                                                 newMonoVar
                                               Just (m, t) -> do t' <- instantiateTy isPoly t
                                                                 return (m, t')
                                                   where isPoly t = not (Set.member t monotyvars)
                                                         monotyvars = Set.unions $ map (tyVarsOf . snd) $ monoVars m
    where newMonoVar = do alpha <- mkTv; return $ name `typedAs` alpha
                              
infer' p (HsLet binds lexpr)               = do p' <- inferLocalBinds p binds
                                                infer p' lexpr

infer' p (OpApp left op fixity right)      = infer' p $ HsApp (genLoc $ HsApp op left) right
-- TODO:
infer' p (NegApp expr negfun)              = error "infer': TODO: NegApp"
infer' p (HsPar lexpr)                     = infer p lexpr

-- inferRhs p (HsUnGuardedRhs expr) = infer p expr
-- inferRhs p (GRHS _ lexpr) = do (ms, ts) <- maptupM infer lexprs
--                                unify ms ts
--     where inferGuardedRhs (HsGuardedRhs srcloc expr guard) = withLoc srcloc $ do (m, t) <- infer p expr
--                                                                                  (m', t') <- infer p guard
--                                                                                  (m'', t'') <- unify [m'] [t', tyBool]
--                                                                                  unify [m, m''] [t]

inferGRhs p (GRHS _ lexpr) = infer p lexpr

inferGRhss p (GRHSs lgrhss _) = do (ms, ts) <- maptupM (inferGRhs p . unLoc) lgrhss
                                   unify ms ts
                             
inferLocalBinds :: PolyEnv -> HsLocalBinds Name -> Stateful PolyEnv
inferLocalBinds p (HsValBinds vb)     = inferValBinds p vb
inferLocalBinds p (HsIPBinds ipbinds) = error "inferLocalBinds: HsIPBnds"
inferLocalBinds p EmptyLocalBinds     = return p                                                           

inferValBinds :: PolyEnv -> HsValBinds Name -> Stateful PolyEnv
inferValBinds p (ValBindsOut recbinds lsigs) = foldM (\ p' lbinds -> inferBinds p' lbinds) p (map snd recbinds)
                                                            
inferBinds :: PolyEnv -> LHsBinds Name -> Stateful PolyEnv
inferBinds p lbinds = do (ms, ts) <- maptupM (inferBind p') binds
                         let definePoly p (name, m, t) = addPolyVar p name (m, t)
                             p'' = foldl definePoly p $ zip3 boundnames ms ts
                         return p''
                               
    where binds = map unLoc $ bagToList lbinds
          boundname FunBind{fun_id = (L _ name)} = name
          boundname VarBind{var_id = name}       = name
          boundnames = map boundname binds
          p' = declareLocals p boundnames

inferBind :: PolyEnv -> (HsBind Name) -> Stateful (MonoEnv, TanType)
inferBind p bind@FunBind{fun_matches = MatchGroup lmatches _} = do (ms, ts) <- maptupM (inferMatch p) matches
                                                                   unify ms ts
    where matches = map unLoc lmatches
               
inferMatch :: PolyEnv -> (Match Name) -> Stateful (MonoEnv, TanType)
inferMatch p (Match lpats _ grhss) = do (ms, ts) <- maptupM (inferPat p . unLoc) lpats
                                        let patternvars = map fst $ concat $ map monoVars ms                                                          
                                            p' = declareLocals (newScope p) $ patternvars
                                        (m, t) <- inferGRhss p' grhss
                                        (m', t') <- unify (m:ms) [tyCurryFun (ts ++ [t])]
                                        return $ (restrictScope p m', t')
          -- inferScope p f = do let p' = newScope p
          --                     (m, t) <- f p
          --                     return $ (restrictScope p m, t)

inferPat :: PolyEnv -> TanPat -> Stateful (MonoEnv, TanType)
inferPat p (AsPat (L _ name) (L _ pat))      = do (m, t) <- inferPat p pat
                                                  return (m |+| (name, t), t)
inferPat p (ParPat (L _ pat))                = inferPat p pat
inferPat p (WildPat _)                       = do alpha <- mkTv
                                                  return $ justType alpha
inferPat p (VarPat name)                     = do alpha <- mkTv
                                                  return $ name `typedAs` alpha
inferPat p (LitPat lit)                      = return $ justType $ typeOfLit lit
inferPat p (ConPatIn (L _ conname) details)  = do tycon <- tyCon p conname
                                                  (ms, ts) <- maptupM (inferPat p) pats
                                                  alpha <- mkTv
                                                  (m, t) <- unify ms [tycon, tyCurryFun (ts ++ [alpha])]
                                                  return (m, last (tyUncurryFun t))
    where pats = case details of
                   PrefixCon lps -> map unLoc lps
                   InfixCon (L _ p) (L _ p') -> [p, p']
inferPat p (TuplePat lpats _ _)              = do (ms, ts) <- maptupM (inferPat p . unLoc) lpats
                                                  return (combineMonos ms, tyTuple ts)
inferPat p (ListPat lpats _)                 = do (ms, ts) <- maptupM (inferPat p . unLoc) lpats
                                                  (m', t') <- unify ms ts
                                                  return (m', tyList t')

                          
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
