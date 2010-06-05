{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- module Tandoori.Ty.Infer(inferValBinds) where
module Tandoori.Ty.Infer where

import Tandoori
import Tandoori.Util
import Tandoori.Ty.State    
import Tandoori.Errors
import Tandoori.Ty
import Tandoori.Ty.Canonize
import Tandoori.Ty.Predicates
import Tandoori.Ty.MonoEnv
import Tandoori.Ty.Ctxt
import Tandoori.Ty.Substitute
import Tandoori.Ty.Unify
import Tandoori.Ty.Instantiate
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List

import Tandoori.GHC.Internals

import Module    
    
import Bag (bagToList)

mkCTv = liftM noPreds mkTv
    
tyCon :: ConName -> Typing CanonizedType
tyCon name  | name == dataConName nilDataCon     = do alpha <- mkCTv
                                                      return $ tyList alpha                                                          
tyCon name  | name == dataConName consDataCon    = do alpha <- mkCTv
                                                      return $ tyCurryFun [alpha, tyList alpha, tyList alpha]
                                                               
--TODO: Clean up builtin constructors mess
tyCon name  | name == dataConName trueDataCon ||
              name == dataConName falseDataCon   = return tyBool
tyCon name                                       = do con <- askCon name
                                                      case con of
                                                        Nothing -> do addError (UndefinedCon name)
                                                                      mkCTv
                                                        Just cty -> instantiateTy (const True) cty
                             
doLoc :: SrcSpan -> Typing a -> Typing a
doLoc srcloc m | isGoodSrcSpan srcloc  = withLoc srcloc $ m
               | otherwise             = m
           
inferLExpr :: Located TanExpr -> Typing (MonoEnv, CanonizedType)
inferLExpr lexpr = withLSrc lexpr $ inferExpr $ unLoc lexpr

typeOfOverLit :: HsOverLit Name -> Typing CanonizedType
typeOfOverLit (OverLit { ol_val = HsIsString _ }) = return $ tyString
typeOfOverLit (OverLit { ol_val = val }) = do alpha <- mkTv
                                              let tc = case val of
                                                         HsIntegral _ -> numClassName
                                                  lpred = genLoc $ HsClassP tc [genLoc alpha]
                                              return $ mkCanonizedType alpha [lpred]
                                              return $ tyInt -- Comment this out to have non-polymorph integer literals
                                                             
inferExpr :: TanExpr -> Typing (MonoEnv, CanonizedType)
inferExpr (HsLit lit)                       = return $ justType $ typeOfLit lit
inferExpr (HsOverLit overlit)               = liftM justType $ typeOfOverLit overlit
inferExpr (HsVar name) | isDataConName name = do ty <- tyCon name
                                                 return $ justType ty                                           
inferExpr (ExplicitList _ lexprs)           = do (ms, ts) <- liftM unzip $ mapM inferLExpr lexprs
                                                 (m, t) <- unify ms ts
                                                 return (m, tyList t)
inferExpr (ExplicitTuple tupargs _ )        = do (ms, ts) <- liftM unzip $ mapM inferTupArg tupargs
                                                 unify ms [tyTuple ts]
    where inferTupArg (Present lexpr) = inferLExpr lexpr                                                                                           

inferExpr (HsApp ltyFun ltyParam)           = do (m1, ty1) <- inferLExpr ltyFun
                                                 (m2, ty2) <- inferLExpr ltyParam
                                                 alpha <- mkCTv
                                                 (m, ty) <- unify [m1, m2] [ty1, tyCurryFun [ty2, alpha]]
                                                 case ctyTy ty of
                                                   HsFunTy (L _ ty3) (L _ ty4) -> return (m, mkCanonizedType ty4 (ctyLPreds ty))
                                                   _                           -> do beta <- mkCTv
                                                                                     return (m, beta)
                                                                 
inferExpr (HsLam (MatchGroup lmatches _))   = do (ms, ts) <- liftM unzip $ mapM inferLMatch lmatches
                                                 unify ms ts

                    -- TODO: move this to askPolyVar, kill askUserDecl
inferExpr (HsVar name) = do decl <- askUserDecl name
                            case decl of
                              Just lty  -> do ty' <- instantiateTy (const True) (unLoc lty)
                                              return $ justType ty'
                              Nothing   -> do pv <- askPolyVar name
                                              case pv of
                                                Nothing -> do alpha <- mkCTv
                                                              return $ name `typedAs` alpha
                                                Just (m, t) -> do monovars <- askForcedMonoVars
                                                                  let isPoly tv = not (tv `Set.member` monotyvars)
                                                                      monotyvars = Set.unions $ map tyVarsOfDef $ Set.toList $ monovars
                                                                  t' <- instantiateTy isPoly t
                                                                  return (m, t')
                                                    where  tyVarsOfDef n = case getMonoVar m n of
                                                                             Nothing -> Set.empty
                                                                             Just ty -> tyVarsOf $ ctyTy ty
                             
inferExpr (HsLet binds lexpr)               = inferLocalBinds binds $ inferLExpr lexpr

inferExpr (OpApp left op fixity right)      = inferExpr $ HsApp (noLoc $ HsApp op left) right
-- TODO:
inferExpr (NegApp expr negfun)              = error "infer': TODO: NegApp"
inferExpr (HsPar lexpr)                     = inferLExpr lexpr

--     where inferGuardedRhs (HsGuardedRhs srcloc expr guard) = withLoc srcloc $ do (m, t) <- infer c expr
--                                                                                  (m', t') <- infer c guard
--                                                                                  (m'', _) <- unify c [m'] [t', tyBool]
--                                                                                  unify c [m, m''] [t]

inferGRhs (GRHS _ lexpr) = inferLExpr lexpr
inferGRhss (GRHSs lgrhss _) = do (ms, ts) <- liftM unzip $ mapM (inferGRhs . unLoc) lgrhss
                                 unify ms ts

-- TODO: vvv Suckage vvv
inferLocalBinds :: HsLocalBinds Name -> Typing (MonoEnv, CanonizedType) -> Typing (MonoEnv, CanonizedType)
inferLocalBinds (HsValBinds vb)      typing  = do ((m, t), vars) <- inferValBinds vb typing
                                                  let isOutsideVisible var _ = not(var `Set.member` vars)
                                                      m' = filterMonoVars isOutsideVisible m
                                                  return (m', t)
inferLocalBinds (HsIPBinds ipbinds)  typing  = error "inferLocalBinds: HsIPBnds"
inferLocalBinds EmptyLocalBinds      typing  = typing

inferValBinds :: HsValBinds Name -> Typing a -> Typing (a, VarSet)
inferValBinds (ValBindsOut recbinds lsigs) typing = withUserDecls lsigs $ runWriterT $ inferBindGroups lbindbags
    where lbindbags = map snd recbinds

          inferBindGroups [] = lift typing
          inferBindGroups (b:bs) = inferBindGroup b $ inferBindGroups bs

          inferBindGroup :: LHsBinds Name -> VarCollector a -> VarCollector a
          inferBindGroup lbindbag cont = do (m, vars) <- listen $ inferBinds lbindbag
                                            tell vars
                                            let varmap = map (\ v -> (v, fromJust $ getMonoVar m v)) $ Set.toList vars
                                            polyvars <- liftM (concatMap maybeToList) $ mapM (lift . toPoly m) varmap
                                            withPolyVars' polyvars $ cont
                                            
              where toPoly m (v, ty) = case lookupDecl v of
                                         Nothing -> do ty' <- resolvePreds ty
                                                       return $ Just (v, (m, ty'))
                                         Just (L loc tyDecl) -> doLoc loc $ do
                                                                  case fitDecl (ctyTy ctyDecl) (ctyTy ty) of
                                                                    Left errs -> do addError $ CantFitDecl ctyDecl ty errs
                                                                                    return Nothing
                                                                    Right s -> do ty' <- resolvePreds (substCTy s (ty, []))
                                                                                  let lpreds = ctyLPreds ty'
                                                                                  ensuresPreds <- ensuresPredicates lpreds ctyDecl'
                                                                                  if ensuresPreds
                                                                                     then return Nothing
                                                                                     else do addError $ CantFitDecl ctyDecl ty' []
                                                                                             return Nothing
                                                                        where ctyDecl' = substCTy s (ctyDecl, [])
                                             where ctyDecl = canonize tyDecl
                                                             
                    lookupDecl v = do (L _ (TypeSig _ lty)) <- find byName lsigs
                                      return lty
                        where byName (L _ (TypeSig (L _ name) _)) = name == v

                    withPolyVars' :: [(VarName, (MonoEnv, CanonizedType))] -> VarCollector a -> VarCollector a
                    withPolyVars' polyvars collector = do (res, vars) <- lift (withPolyVars polyvars $ runWriterT collector)
                                                          tell vars
                                                          return res
-- TODO: ^^^ Suckage ^^^
                                                                    
inferBinds :: LHsBinds Name -> VarCollector MonoEnv
inferBinds lbindbag = do let lbinds = bagToList lbindbag
                         ms <- mapM inferLBind lbinds
                         (m, _) <- lift $ unify ms []
                         return m
              
type VarSet = Set.Set VarName    
type VarCollector a = WriterT VarSet Typing a

withLSrc' l collect = do (res, vars) <- lift $ withLSrc l $ runWriterT collect
                         tell vars
                         return res
    
inferLBind :: (LHsBind Name) -> VarCollector MonoEnv
inferLBind lbind = withLSrc' lbind $ inferBind $ unLoc lbind
                                                                                        
inferBind :: (HsBind Name) -> VarCollector MonoEnv
inferBind PatBind{pat_lhs = lpat, pat_rhs = grhss} = do (m, t) <- inferLPat lpat
                                                        (m', t') <- lift $ inferGRhss grhss
                                                        (m'', _) <- lift $ unify [m, m'] [t, t']
                                                        return m''
inferBind VarBind{} = error "VarBind"
inferBind FunBind{fun_matches = MatchGroup lmatches _, fun_id = (L _ name)} = do tell $ Set.singleton name
                                                                                 (ms, ts) <- lift $ liftM unzip $ mapM inferLMatch lmatches
                                                                                 (m, t) <- lift $ unify ms ts
                                                                                 return $ addMonoVar m (name, t)

inferLMatch :: (LMatch Name) -> Typing (MonoEnv, CanonizedType)
inferLMatch lmatch = doLoc (getLoc lmatch) $ inferMatch $ unLoc lmatch
                               
inferMatch :: (Match Name) -> Typing (MonoEnv, CanonizedType)
inferMatch (Match lpats _ grhss) = do ((ms, ts), vars) <- runWriterT $ liftM unzip $ mapM inferLPat lpats
                                      (m, t) <- withMonoVars vars $ inferGRhss grhss
                                      (m', t') <- unify (m:ms) [tyCurryFun (ts ++ [t])]
                                      let m'' = filterMonoVars (\ v ty -> not (Set.member v vars)) m'
                                      return $ (m'', t')


                                             
inferLPat :: Located (Pat Name) -> VarCollector (MonoEnv, CanonizedType)
inferLPat lpat = withLSrc' lpat $ inferPat $ unLoc lpat
                                     
inferPat :: Pat Name -> VarCollector (MonoEnv, CanonizedType)
inferPat (AsPat (L _ name) lpat)           = do (m, t) <- inferLPat lpat
                                                let m' = addMonoVar m (name, t)
                                                return (m', t)
inferPat (ParPat lpat)                     = inferLPat lpat
inferPat (WildPat _)                       = do alpha <- lift mkCTv
                                                return $ justType $ alpha
inferPat (VarPat name)                     = do tell $ Set.singleton name
                                                alpha <- lift mkCTv
                                                return $ name `typedAs` alpha
inferPat (LitPat lit)                      = return $ justType $ typeOfLit lit
inferPat (ConPatIn (L _ conname) details)  = do tycon <- lift $ tyCon conname
                                                (ms, ts) <- liftM unzip $ mapM inferLPat lpats
                                                alpha <- lift mkCTv
                                                (m, t) <- lift $ unify ms [tycon, tyCurryFun (ts ++ [alpha])]
                                                return $ (m, tyFunResult t)
    where lpats = case details of
                    PrefixCon lpats -> lpats
                    InfixCon lp lp' -> [lp, lp']
          tyFunResult cty = mkCanonizedType tyResult (ctyLPreds cty)
              where tyResult = tyRightmost $ ctyTy cty
          tyRightmost (HsFunTy (L _ tyLeft) (L _ tyRight)) = tyRightmost tyRight
          tyRightmost ty                                   = ty
                                                 
inferPat (TuplePat lpats _ _)              = do (ms, ts) <- liftM unzip $ mapM inferLPat lpats
                                                return (combineMonos ms, tyTuple ts)
inferPat (ListPat lpats _)                 = do (ms, ts) <- liftM unzip $ mapM inferLPat lpats
                                                (m', t') <- lift $ unify ms ts
                                                return (m', tyList t')
inferPat (NPat overlit _ _)                = liftM justType $ lift $ typeOfOverLit overlit

                          
unify :: [MonoEnv] -> [CanonizedType] -> Typing (MonoEnv, CanonizedType)
unify ms tys = do eqs <- monoeqs
                  alpha <- mkTv
                  let eqs' = map (\ ty -> (alpha :=: ctyTy ty)) tys
                      calpha = mkCanonizedType alpha (concatMap ctyLPreds tys)
                  case mgu (eqs ++ eqs') of
                    Left errs -> do addError $ UnificationFailed ms errs
                                    return $ (combineMonos ms, noPreds alpha)
                    Right s -> do let ms' = map (xformMono s) ms
                                      ty' = xformTy s calpha
                                  return $ (combineMonos ms', ty')
                                  
    where monoeqs = do tyvarmap <- liftM Map.fromList $ mapM mkVar varnames
                       let mkEq (var, ty) = (fromJust $ Map.lookup var tyvarmap) :=: ctyTy ty
                       return $ map mkEq vars
                              
              where varnames = distinct $ map fst vars
                    distinct = Set.toList . Set.fromList
                    mkVar var = do tv <- mkTv
                                   return (var, tv)
                               
          vars = concat $ map getMonoVars ms
          monolpreds = concatMap (ctyLPreds . snd) vars
                     
          xformMono s m = mapMono (xformTy s) m
          xformTy s ty = substCTy s (ty, monolpreds)
