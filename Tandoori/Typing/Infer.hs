{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- module Tandoori.Ty.Infer(inferValBinds) where
module Tandoori.Typing.Infer where

import Tandoori
import Tandoori.Util
import Tandoori.Typing
import Tandoori.Typing.Monad
import Tandoori.Errors
import Tandoori.Typing.MonoEnv
import Tandoori.Typing.Ctxt
import Tandoori.Typing.Unify
import Tandoori.Typing.Substitute
import Tandoori.Typing.Instantiate
import Tandoori.Typing.DataType
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error
import Control.Applicative
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List

import Debug.Trace    
    
import Tandoori.GHC.Internals

import Module    
    
import Bag (bagToList)

infer decls group = runTyping $ do
                      case (liftM concat $ mapM constructorsFromDecl decls) of
                        Left err -> error "cons error" -- TODO: error
                        Right cons -> withCons cons $ do
                                        liftM fst $ inferValBinds (hs_valds group)

                             
doLoc :: SrcSpan -> Typing a -> Typing a
doLoc srcloc m | isGoodSrcSpan srcloc  = withLoc srcloc $ m
               | otherwise             = m
           
typeOfOverLit :: HsOverLit Name -> Typing PolyTy
typeOfOverLit (OverLit { ol_val = HsIsString _ }) = return $ PolyTy [] tyString
typeOfOverLit (OverLit { ol_val = val }) = do α <- mkTv
                                              let cls = case val of
                                                          HsIntegral _ -> numClassName
                                              return $ PolyTy [(cls, α)] (TyVar α)
                                              return $ PolyTy [] tyInt -- Comment this out to have non-polymorph integer literals
                                                             
inferLExpr :: Located TanExpr -> Typing (MonoEnv, PolyTy)
inferLExpr lexpr = withLSrc lexpr $ inferExpr $ unLoc lexpr

inferExpr :: HsExpr Name -> Typing (MonoEnv, PolyTy)
inferExpr (HsLit lit)                       = return $ justType $ PolyTy [] $ typeOfLit lit
inferExpr (HsOverLit overlit)               = liftM justType $ typeOfOverLit overlit
inferExpr (HsVar name) | isDataConName name = do con <- askCon name
                                                 τ <- case con of
                                                       Nothing -> do addError (UndefinedCon name)
                                                                     mkTyVar
                                                       Just τ -> instantiate (const True) τ
                                                 return $ justType $ PolyTy [] τ
inferExpr (ExplicitList _ lexprs)           = do (ms, ts) <- liftM unzip $ mapM inferLExpr lexprs
                                                 (m, t) <- unify ms ts
                                                 return (m, ptyList t)
inferExpr (ExplicitTuple tupargs _ )        = do (ms, ts) <- liftM unzip $ mapM inferTupArg tupargs
                                                 unify ms [tyTuple ts]
    where inferTupArg (Present lexpr) = inferLExpr lexpr                                                                                           

inferExpr (HsApp ltyFun ltyParam)           = do (m1, σ1) <- inferLExpr ltyFun
                                                 (m2, σ2) <- inferLExpr ltyParam
                                                 alpha <- mkTyVar
                                                 (m, σ) <- unify [m1, m2] [σ1, ptyCurryFun [σ2, PolyTy [] alpha]]
                                                 case σ of
                                                   (PolyTy ctx (TyFun τ3 τ4)) -> return (m, PolyTy ctx τ4)
                                                   _                           -> do -- TODO: Error
                                                                                     beta <- mkTyVar
                                                                                     return (m, PolyTy [] beta)
                                                                 
inferExpr (HsLam (MatchGroup lmatches _))   = do (ms, σs) <- liftM unzip $ mapM inferLMatch lmatches
                                                 unify ms σs

                     -- TODO: move this to askPolyVar, kill askUserDecl
inferExpr (HsVar x) = do decl <- askUserDecl x
                         case decl of
                           Just lσ  -> do σ' <- instantiatePolyTy (const True) (unLoc lσ)
                                          return $ justType σ'
                           Nothing   -> do pv <- askPolyVar x
                                           case pv of
                                             Nothing -> do monovars <- askForcedMonoVars
                                                           unless (x `Set.member` monovars) $
                                                                  addError $ UndefinedVar x
                                                           alpha <- mkTyVar
                                                           return $ x `typedAs` (PolyTy [] alpha)
                                             Just (m, σ) -> do monovars <- askForcedMonoVars
                                                               let isPoly tv = not (tv `Set.member` monotyvars)
                                                                   monotyvars = Set.unions $ map tvsOfDef $ Set.toList $ monovars
                                                               σ' <- instantiatePolyTy isPoly σ
                                                               return (m, σ') -- TODO: Instantiate m as well
                                                 where  tvsOfDef n = case getMonoVar m n of
                                                                       Nothing -> Set.empty
                                                                       Just (PolyTy _ τ) -> tvsOf τ
                             
inferExpr (HsLet binds lexpr)               = do (ctxt, vars) <- inferLocalBinds binds
                                                 (m, σ) <- withCtxt ctxt $ inferLExpr lexpr
                                                 let isOutsideVisible var _ = not(var `Set.member` vars)
                                                     m' = filterMonoVars isOutsideVisible m
                                                 return (m', σ)

inferExpr (OpApp left op fixity right)      = inferExpr $ HsApp (noLoc $ HsApp op left) right
-- TODO:
inferExpr (NegApp expr negfun)              = error "infer': TODO: NegApp"
inferExpr (HsPar lexpr)                     = inferLExpr lexpr

-- --     where inferGuardedRhs (HsGuardedRhs srcloc expr guard) = withLoc srcloc $ do (m, t) <- infer c expr
-- --                                                                                  (m', t') <- infer c guard
-- --                                                                                  (m'', _) <- unify c [m'] [t', tyBool]
-- --                                                                                  unify c [m, m''] [t]

inferLocalBinds :: HsLocalBinds Name -> Typing (Ctxt, VarSet)
inferLocalBinds (HsValBinds vb)      = inferValBinds vb
inferLocalBinds (HsIPBinds ipbinds)  = error "inferLocalBinds: HsIPBnds"
inferLocalBinds EmptyLocalBinds      = do ctxt <- askCtxt
                                          return (ctxt, Set.empty)

inferValBinds :: HsValBinds Name -> Typing (Ctxt, VarSet)
inferValBinds (ValBindsOut recbinds lsigs) = do decls <- liftM catMaybes $ mapM fromSig lsigs
                                                withUserDecls decls $ runWriterT $ inferBindGroups lbindbags
    where lbindbags = map snd recbinds

          fromSig (L srcloc (TypeSig (L _ name) (L _ ty))) = do τ <- case fromHsType ty of
                                                                  Right τ -> return τ
                                                                  Left err -> error "fromHsType failed" -- TODO: error
                                                                return $ Just $ (name, L srcloc τ)
          fromSig _ = return Nothing

          inferBindGroups [] = lift $ askCtxt
          inferBindGroups (b:bs) = do ctxt' <- inferBindGroup b
                                      sinkWriter (withCtxt ctxt') $ inferBindGroups bs

          inferBindGroup :: LHsBinds Name -> VarCollector Ctxt
          inferBindGroup lbindbag = do (m, vars) <- listen $ inferBinds lbindbag
                                       tell vars
                                       let varmap = map (\ v -> (v, fromJust $ getMonoVar m v)) $ Set.toList vars
                                       polyvars <- liftM catMaybes $ mapM (lift . toPoly m) varmap
                                       addPolyVars' polyvars
              where toPoly m (v, σ) = case lookupDecl v of
                                         Nothing -> do -- checkCtxAmbiguity
                                                       return $ Just (v, (m, σ))
                                         Just (L loc σDecl) -> doLoc loc $ do
                                                                  fitDecl σDecl σ
                                                                  return Nothing
                                                                  -- case fit of
                                                                  --   Left errs -> do -- addError $ CantFitDecl σDecl σ errs -- TODO: errors
                                                                  --                   return Nothing
                                                                  --   Right s -> do σ' <- resolvePreds (substCTy s (ty, []))
                                                                  --                 let lpreds = ctyLPreds ty'
                                                                  --                 ensuresPreds <- ensuresPredicates lpreds ctyDecl'
                                                                  --                 if ensuresPreds
                                                                  --                    then return Nothing
                                                                  --                    else do addError $ CantFitDecl ctyDecl ty' []
                                                                  --                            return Nothing
                                                                  --       where ctyDecl' = substCTy s (ctyDecl, [])


                    fitDecl σDecl σ = error "undefined: fitDecl"
                                                             
                    lookupDecl v = do (L _ (TypeSig _ lty)) <- find byName lsigs
                                      return lty
                        where byName (L _ (TypeSig (L _ name) _)) = name == v

                    addPolyVars' :: [(VarName, (MonoEnv, PolyTy))] -> VarCollector Ctxt
                    addPolyVars' polyvars = do ctxt <- lift askCtxt
                                               let ctxt' = addPolyVars ctxt polyvars
                                               return ctxt'                                                        
                                                                    
inferBinds :: LHsBinds Name -> VarCollector MonoEnv
inferBinds lbindbag = do let lbinds = bagToList lbindbag
                         ms <- mapM inferLBind lbinds
                         (m, _) <- lift $ unify ms []
                         return m
              
type VarSet = Set.Set VarName    
type VarCollector a = WriterT VarSet Typing a

withLSrc' l collect = sinkWriter (withLSrc l) $ collect
    
inferLBind :: (LHsBind Name) -> VarCollector MonoEnv
inferLBind lbind = withLSrc' lbind $ inferBind $ unLoc lbind
                                                                                        
inferBind :: (HsBind Name) -> VarCollector MonoEnv
inferBind PatBind{pat_lhs = lpat, pat_rhs = grhss} = do ((m, σ), vars) <- listen $ inferLPat lpat -- TODO: add new vars to ctxt
                                                        tell vars
                                                        (m', σ') <- lift $ withMonoVars vars $ inferGRhss grhss
                                                        (m'', _) <- lift $ unify [m, m'] [σ, σ']
                                                        return m''
inferBind VarBind{} = error "VarBind"
inferBind FunBind{fun_matches = MatchGroup lmatches _, fun_id = (L _ name)} = do tell $ Set.singleton name
                                                                                 (ms, ts) <- lift $ withMonoVars (Set.singleton name) $ liftM unzip $ mapM inferLMatch lmatches
                                                                                 (m, t) <- lift $ unify ms ts
                                                                                 return $ addMonoVar m (name, t)

inferLMatch :: (LMatch Name) -> Typing (MonoEnv, PolyTy)
inferLMatch lmatch = doLoc (getLoc lmatch) $ inferMatch $ unLoc lmatch
                               
inferMatch :: (Match Name) -> Typing (MonoEnv, PolyTy)
inferMatch (Match lpats _ grhss) = do ((ms, σs), vars) <- runWriterT $ liftM unzip $ mapM inferLPat lpats
                                      (m, σ) <- withMonoVars vars $ inferGRhss grhss
                                      (m', σ') <- unify (m:ms) [ptyCurryFun (σs ++ [σ])]
                                      let m'' = filterMonoVars (\ v ty -> not (Set.member v vars)) m'
                                      return $ (m'', σ')


inferGRhs (GRHS _ lexpr) = inferLExpr lexpr
inferGRhss (GRHSs lgrhss _) = do (ms, σs) <- liftM unzip $ mapM (inferGRhs . unLoc) lgrhss
                                 unify ms σs

                                             
inferLPat :: Located (Pat Name) -> VarCollector (MonoEnv, PolyTy)
inferLPat lpat = withLSrc' lpat $ inferPat $ unLoc lpat
                                     
inferPat :: Pat Name -> VarCollector (MonoEnv, PolyTy)
inferPat (AsPat (L _ name) lpat)           = do tell $ Set.singleton name
                                                (m, σ) <- inferLPat lpat                                                
                                                let m' = addMonoVar m (name, σ)
                                                return (m', σ)
inferPat (ParPat lpat)                     = inferLPat lpat
inferPat (WildPat _)                       = do alpha <- lift mkTyVar
                                                return $ justType $ (PolyTy [] alpha)
inferPat (VarPat name)                     = do tell $ Set.singleton name
                                                alpha <- lift mkTyVar
                                                return $ name `typedAs` (PolyTy [] alpha)
inferPat (LitPat lit)                      = return $ justType $ PolyTy [] $ typeOfLit lit
inferPat (ConPatIn (L _ con) details)  = do Just τCon <- lift $ askCon con -- TODO: errors
                                            (ms, σs) <- liftM unzip $ mapM inferLPat lpats
                                            alpha <- lift mkTyVar
                                            (m, σ) <- lift $ unify ms [PolyTy [] τCon, ptyCurryFun (σs ++ [PolyTy [] alpha])]
                                            return $ (m, ptyFunResult σ)
    where lpats = case details of
                    PrefixCon lpats -> lpats
                    InfixCon lp lp' -> [lp, lp']
          ptyFunResult (PolyTy ctx τ) = PolyTy ctx (tyRightmost τ)
          tyRightmost (TyFun τ1 τ2) = tyRightmost τ2
          tyRightmost τ             = τ
                                                 
inferPat (TuplePat lpats _ _)              = do (ms, σs) <- liftM unzip $ mapM inferLPat lpats
                                                return (combineMonos ms, ptyTuple σs)
inferPat (ListPat lpats _)                 = do (ms, σs) <- liftM unzip $ mapM inferLPat lpats
                                                (m', σ') <- lift $ unify ms σs
                                                return (m', ptyList σ')
inferPat (NPat overlit _ _)                = liftM justType $ lift $ typeOfOverLit overlit

                          
unify :: [MonoEnv] -> [PolyTy] -> Typing (MonoEnv, PolyTy)
unify ms σs = do eqs <- monoeqs
                 α <- mkTyVar
                 let eqs' = map (\ (PolyTy _ τ) -> (α :=: τ)) σs
                     σ = PolyTy (concatMap getCtx σs) α
                 u <- runErrorT $ mgu $ eqs ++ eqs'
                 case u of
                   Left err -> do error "TODO: errors"
                                  -- addError $ UnificationFailed ms eqs
                                  return $ (combineMonos ms, PolyTy [] α)
                   Right s -> do ms' <- mapM (substMono s) ms
                                 σ' <- subst s σ
                                 return (combineMonos ms', σ')
                        
    where getCtx (PolyTy ctx _) = ctx

          monoeqs = do tyvarmap <- liftM Map.fromList $ mapM mkVar varnames
                       let mkEq (var, (PolyTy _ τ)) = (fromJust $ Map.lookup var tyvarmap) :=: τ
                       return $ map mkEq vars
              where varnames = nub $ map fst vars
                    mkVar var = do tv <- mkTyVar
                                   return (var, tv)

          vars = concatMap getMonoVars ms
          substMono s m = mapMonoM (subst s) m
                                  

---- Predicates
substPred :: Subst -> PolyPred -> Typing [PolyPred]
substPred s (cls, α) = resolvePred (cls, substTy s (TyVar α))

substCtx :: Subst -> PolyCtx -> Typing PolyCtx
substCtx s ctx = concat <$> mapM (substPred s) ctx
          
subst :: Subst -> PolyTy -> Typing PolyTy
subst s (PolyTy ctx τ) = do let τ' = substTy s τ
                            ctx' <- substCtx s ctx     
                            return $ PolyTy ctx' τ'
                                   
resolvePred :: OverPred -> Typing PolyCtx
resolvePred = error "undefined: resolvePred"
-- resolvePred (cls, τ) = case τ of
--                          TyVar α -> return [(cls, α)]
--                          _       -> do let κ = fromJust $ tyCon τ
--                                        instData <- askInstance cls κ
--                                        case instData of
--                                          Nothing -> throwError $ MissingInstance (cls, τ)
--                                          Just (PolyTy ctx τ') -> do s <- unify [τ' :=: τ] -- N.b. the order of the equation is important
--                                                                     substCtx s (fromPolyCtx ctx)
