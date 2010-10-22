{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tandoori.Typing.Infer(infer) where
-- module Tandoori.Typing.Infer where

import Tandoori
import Tandoori.Typing
import Tandoori.Typing.Monad
import Tandoori.Typing.Error
import Tandoori.Typing.MonoEnv
import Tandoori.Typing.Ctxt
import Tandoori.Typing.Unify
import Tandoori.Typing.Substitute
import Tandoori.Typing.Instantiate
import Tandoori.Typing.DataType
import Tandoori.Typing.ClassDecl
import Tandoori.Typing.InstanceDecl
import Tandoori.Typing.Repr
    
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Maybe
import Control.Applicative
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List

import Tandoori.Typing.Show
import Debug.Trace    
    
import Tandoori.GHC.Internals

import Bag (bagToList)
    
infer decls group = runTyping $ do
                      cons <- concat <$> mapM constructorsFromDecl decls
                      withCons cons $ do
                        cis <- classMap decls
                        withClasses cis $ do
                          insts <- mapM getInstance (hs_instds group)
                          withInstances insts $ do
                            ctxt <- fst <$> inferValBinds (hs_valds group)
                            withCtxt ctxt $ do
                              mapM_ checkLInstance (hs_instds group)
                            return ctxt

    where getInstance linst = withLSrc linst $ instDecl (unLoc linst)
          checkLInstance linst = withLSrc linst $ checkInstance (unLoc linst)

          checkInstance inst@(InstDecl lty binds lsigs _)  = do
            ((cls, κ), σ) <- instDecl inst
            let PolyTy ctx τ = σ
            PolyTy ctx' τ' <- instantiatePolyTy (const True) σ
            ci <- askClass cls
            let checkSuper cls' = withLSrc lty $ do
                  ctx' <- resolvePred (cls', τ)
                  superOK <- ctx `satisfies` ctx'
                  unless superOK $ do
                    raiseError $ MissingBaseInstances (cls, τ) ctx'
                checkMember lbind = return ()
            mapM_ checkSuper (clsSupers ci)
            (ctxt', vars) <- listenVars $ inferBindGroup binds lsigs
            return ()
                              
doLoc :: SrcSpan -> Typing a -> Typing a
doLoc srcloc m | isGoodSrcSpan srcloc  = withLoc srcloc $ m
               | otherwise             = m
           
typeOfOverLit :: HsOverLit Name -> Typing PolyTy
typeOfOverLit (OverLit { ol_val = HsIsString _ }) = return $ PolyTy [] tyString
typeOfOverLit (OverLit { ol_val = val }) = do α <- mkTv
                                              let cls = case val of
                                                          HsIntegral _ -> numClassName
                                              return $ PolyTy [(cls, α)] (TyVar α)
                                              -- return $ PolyTy [] tyInt -- Comment this out to have non-polymorph integer literals
                                                             
inferLExpr :: Located TanExpr -> Typing (MonoEnv, PolyTy)
inferLExpr lexpr = withLSrc lexpr $ inferExpr $ unLoc lexpr

inferExpr :: HsExpr Name -> Typing (MonoEnv, PolyTy)
inferExpr (HsLit lit)                       = noVars ⊢ (PolyTy [] $ typeOfLit lit)
inferExpr (HsOverLit overlit)               = do σ <- typeOfOverLit overlit
                                                 noVars ⊢ σ
inferExpr (HsVar name) | isDataConName name = do τ <- (instantiate (const True) =<< askCon name) `orRecover` mkTyVar
                                                 noVars ⊢ PolyTy [] τ
inferExpr (ExplicitList _ lexprs)           = do (ms, σs) <- unzip <$> mapM inferLExpr lexprs
                                                 (m, σ) <- unify ms σs
                                                 m ⊢ ptyList σ
inferExpr (ExplicitTuple tupargs _ )        = do (ms, σs) <- unzip <$> mapM inferTupArg tupargs
                                                 (m, σ) <- unify ms [ptyTuple σs]
                                                 m ⊢ σ
    where inferTupArg (Present lexpr) = inferLExpr lexpr                                                                                           

inferExpr (HsApp ltyFun ltyParam)           = do (m1, σ1) <- inferLExpr ltyFun
                                                 (m2, σ2) <- inferLExpr ltyParam
                                                 α <- mkTyVar
                                                 (m, σ) <- unify [m1, m2] [σ1, ptyCurryFun [σ2, PolyTy [] α]]
                                                 case σ of
                                                   (PolyTy ctx (TyFun τ3 τ4)) -> m ⊢ PolyTy ctx τ4
                                                   _                          -> do -- TODO: Error
                                                                                    β <- mkTyVar
                                                                                    m ⊢ PolyTy [] β
                                                                 
inferExpr (HsLam (MatchGroup lmatches _))   = do (ms, σs) <- unzip <$> mapM inferLMatch lmatches
                                                 (m, σ) <- unify ms σs
                                                 m ⊢ σ

-- TODO: move this to askPolyVar, kill askUserDecl
inferExpr (HsVar x) = do decl <- askUserDecl x
                         case decl of
                           Just lσ  -> do σ' <- instantiatePolyTy (const True) (unLoc lσ)
                                          noVars ⊢ σ'
                           Nothing   -> do pv <- askPolyVar x
                                           case pv of
                                             Nothing -> do monovars <- askForcedMonoVars
                                                           unless (x `Set.member` monovars) $
                                                                   addError $ UndefinedVar x
                                                           alpha <- mkTyVar
                                                           x `typedAs` (PolyTy [] alpha)
                                             Just (m, σ) -> do monovars <- askForcedMonoVars
                                                               let isPoly tv = not (tv `Set.member` monotyvars)
                                                                   monotyvars = Set.unions $ map tvsOfDef $ Set.toList $ monovars
                                                               σ' <- instantiatePolyTy isPoly σ
                                                               m ⊢ σ' -- TODO: Instantiate m as well?
                                                 where tvsOfDef n = case getMonoVar m n of
                                                                      Nothing -> Set.empty
                                                                      Just (PolyTy _ τ) -> tvsOf τ
                             
inferExpr (HsLet binds lexpr)               = do (ctxt, vars) <- inferLocalBinds binds
                                                 (m, σ) <- withCtxt ctxt $ inferLExpr lexpr
                                                 let isOutsideVisible var _ = not(var `Set.member` vars)
                                                     m' = filterMonoVars isOutsideVisible m
                                                 m' ⊢ σ

inferExpr (OpApp left op fixity right)      = inferExpr $ HsApp (noLoc $ HsApp op left) right
-- TODO:
inferExpr (NegApp expr negfun)              = error "infer': TODO: NegApp"
inferExpr (HsPar lexpr)                     = inferLExpr lexpr

inferLocalBinds :: HsLocalBinds Name -> Typing (Ctxt, VarSet)
inferLocalBinds (HsValBinds vb)      = inferValBinds vb
inferLocalBinds (HsIPBinds ipbinds)  = error "inferLocalBinds: HsIPBnds"
inferLocalBinds EmptyLocalBinds      = do ctxt <- askCtxt
                                          return (ctxt, Set.empty)

inferValBinds :: HsValBinds Name -> Typing (Ctxt, VarSet)
inferValBinds (ValBindsOut recbinds lsigs) = do decls <- catMaybes <$> mapM fromSig lsigs
                                                withUserDecls decls $ listenVars $ inferBindGroups lbindbags
    where lbindbags = map snd recbinds

          fromSig (L srcloc (TypeSig (L _ name) (L _ ty))) = do τ <- fromHsType ty
                                                                return $ Just $ (name, L srcloc τ)
          fromSig _ = return Nothing

          inferBindGroups [] = askCtxt
          inferBindGroups (b:bs) = do ctxt' <- inferBindGroup b lsigs
                                      withCtxt ctxt' $ inferBindGroups bs

fromJust' (Nothing) = error "foo"
fromJust' (Just x) = x

inferBindGroup :: LHsBinds Name -> [LSig Name] -> Typing Ctxt
inferBindGroup lbindbag lsigs = do (m, vars) <- listenVars $ inferBinds lbindbag
                                   tellVars vars
                                   let varmap = map (\ v -> (v, fromJust' $ getMonoVar m v)) $ Set.toList vars
                                   -- TODO: Location
                                   polyvars <- catMaybes <$> mapM (toPoly m) varmap
                                   addPolyVars' polyvars
    where toPoly m (v, σ) = do lookup <- runMaybeT $ lookupDecl v
                               case lookup of
                                 Nothing -> do checkCtxAmbiguity σ
                                               return $ Just (v, (m, σ))
                                 Just (L loc σDecl) -> doLoc loc $ do
                                                         checkDecl σDecl σ
                                                         checkCtxAmbiguity σDecl
                                                         return Nothing

          checkCtxAmbiguity σ@(PolyTy ctx τ) = forM_ ctx $ \(cls, α) -> do
                                                 unless (α `Set.member` tvs) $
                                                   raiseError $ AmbiguousPredicate σ (cls, α)
              where tvs = tvsOf τ
                                                                           
          checkDecl σDecl@(PolyTy ctxDecl τDecl) σ@(PolyTy ctx τ) = do
            fit <- runErrorT $ fitDeclTy τDecl τ
            case fit of
              Left err -> raiseError $ CantFitDecl σDecl σ
              Right s -> do σ'@(PolyTy ctx' _) <- subst s σ
                            ctxOk <- ctxDecl `satisfies` ctx'
                            unless ctxOk $ do
                              raiseError $ CantFitDecl σDecl σ'

          lookupDecl :: VarName -> MaybeT Typing (Located PolyTy)
          lookupDecl v = do (L loc (TypeSig _ lty)) <- MaybeT $ return $ find byName lsigs
                            σDecl <- lift $ fromHsType $ unLoc lty
                            return $ L loc σDecl
              where byName (L _ (TypeSig (L _ name) _)) = name == v

          addPolyVars' :: [(VarName, (MonoEnv, PolyTy))] -> Typing Ctxt
          addPolyVars' polyvars = do ctxt <- askCtxt
                                     let ctxt' = addPolyVars ctxt polyvars
                                     return ctxt'                                                        
                                                                    
inferBinds :: LHsBinds Name -> Typing MonoEnv
inferBinds lbindbag = do let lbinds = bagToList lbindbag
                         ms <- mapM inferLBind lbinds
                         (m, _) <- unify ms []
                         return m
              
inferLBind :: (LHsBind Name) -> Typing MonoEnv
inferLBind lbind = withLSrc lbind $ inferBind $ unLoc lbind
                                                                                        
inferBind :: (HsBind Name) -> Typing MonoEnv
inferBind PatBind{pat_lhs = lpat, pat_rhs = grhss} = do ((m, σ), vars) <- listenVars $ inferLPat lpat -- TODO: add new vars to ctxt
                                                        tellVars vars
                                                        (m', σ') <- withMonoVars vars $ inferGRhss grhss
                                                        (m'', _) <- unify [m, m'] [σ, σ']
                                                        return m''
inferBind VarBind{} = error "VarBind"
inferBind FunBind{fun_matches = MatchGroup lmatches _, fun_id = (L _ name)} = do tellVar name
                                                                                 (ms, σs) <- withMonoVars (Set.singleton name) $ censorVars $ 
                                                                                            unzip <$> mapM inferLMatch lmatches
                                                                                 (m, σ) <- unify ms σs
                                                                                 return $ addMonoVar m (name, σ)

inferLMatch :: (LMatch Name) -> Typing (MonoEnv, PolyTy)
inferLMatch lmatch = doLoc (getLoc lmatch) $ inferMatch $ unLoc lmatch
                               
inferMatch :: (Match Name) -> Typing (MonoEnv, PolyTy)
inferMatch (Match lpats _ grhss) = do ((ms, σs), vars) <- listenVars $ (unzip <$> mapM inferLPat lpats)
                                      (m, σ) <- withMonoVars vars $ inferGRhss grhss
                                      (m', σ') <- unify (m:ms) [ptyCurryFun (σs ++ [σ])]
                                      let m'' = filterMonoVars (\ v ty -> not (Set.member v vars)) m'
                                      m'' ⊢ σ'


inferGRhs (GRHS _ lexpr) = inferLExpr lexpr
inferGRhss (GRHSs lgrhss _) = do (ms, σs) <- unzip <$> mapM (inferGRhs . unLoc) lgrhss
                                 (m, σ) <- unify ms σs
                                 m ⊢ σ

                                             
inferLPat :: Located (Pat Name) -> Typing (MonoEnv, PolyTy)
inferLPat lpat = withLSrc lpat $ inferPat $ unLoc lpat
                                     
inferPat :: Pat Name -> Typing (MonoEnv, PolyTy)
inferPat (AsPat (L _ name) lpat)           = do tellVar name
                                                (m, σ) <- inferLPat lpat                                                
                                                let m' = addMonoVar m (name, σ)
                                                m' ⊢ σ
inferPat (ParPat lpat)                     = inferLPat lpat
inferPat (WildPat _)                       = do α <- mkTyVar
                                                noVars ⊢ PolyTy [] α
inferPat (VarPat name)                     = do tellVar name
                                                alpha <- mkTyVar
                                                name `typedAs` (PolyTy [] alpha)
inferPat (LitPat lit)                      = noVars ⊢ (PolyTy [] $ typeOfLit lit)
inferPat (ConPatIn (L _ con) details)  = do τCon <- askCon con -- TODO: errors
                                            (ms, σs) <- unzip <$> mapM inferLPat lpats
                                            alpha <- mkTyVar
                                            (m, σ) <- unify ms [PolyTy [] τCon, ptyCurryFun (σs ++ [PolyTy [] alpha])]
                                            m ⊢ ptyFunResult σ
    where lpats = case details of
                    PrefixCon lpats -> lpats
                    InfixCon lp lp' -> [lp, lp']
          ptyFunResult (PolyTy ctx τ) = PolyTy ctx (tyRightmost τ)
          tyRightmost (TyFun τ1 τ2) = tyRightmost τ2
          tyRightmost τ             = τ
                                                 
inferPat (TuplePat lpats _ _)              = do (ms, σs) <- unzip <$> mapM inferLPat lpats
                                                combineMonos ms ⊢ ptyTuple σs
inferPat (ListPat lpats _)                 = do (ms, σs) <- unzip <$> mapM inferLPat lpats
                                                (m, σ) <- unify ms σs
                                                m ⊢ ptyList σ
inferPat (NPat overlit _ _)                = do σ <- typeOfOverLit overlit
                                                noVars ⊢ σ
                                             
infix 1 ⊢
(⊢) :: MonoEnv -> PolyTy -> Typing (MonoEnv, PolyTy)                                             
m ⊢ σ = do let m' = setMonoTy m σ
           src <- askSrc
           let m'' = case src of
                 Nothing -> m'
                 Just src -> setMonoSrc m' src
           return (m'', σ)
                                             
typedAs :: VarName -> PolyTy -> Typing (MonoEnv, PolyTy)
v `typedAs` σ = (addMonoVar noVars (v, σ)) ⊢ σ


unify :: [MonoEnv] -> [PolyTy] -> Typing (MonoEnv, PolyTy)
unify ms σs = do eqs <- monoeqs
                 α <- mkTyVar
                 let eqs' = map (\ (PolyTy _ τ) -> (α :=: τ)) σs
                     σ = PolyTy (concatMap getCtx σs) α
                 u <- runErrorT $ mgu $ eqs ++ eqs'
                 case u of
                   Left err -> raiseError $ UnificationFailed ms err
                   Right s -> do ms' <- mapM (substMono s) ms
                                 σ' <- subst s σ
                                 return (combineMonos ms', σ')
                  `orRecover` (return (combineMonos ms, PolyTy [] α))
                        
    where getCtx (PolyTy ctx _) = ctx

          monoeqs = do tyvarmap <- Map.fromList <$> mapM mkVar varnames
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
                            ctx' <- simplifyCtx =<< substCtx s ctx     
                            return $ PolyTy ctx' τ'
                                   
resolvePred :: OverPred -> Typing PolyCtx
resolvePred (cls, τ) = case τ of
                         TyVar α -> return [(cls, α)]
                         τ       -> do let κ = fromJust $ tyCon τ
                                       instData <- askInstance cls κ
                                       case instData of
                                         Nothing -> raiseError $ UnfulfilledPredicate (cls, τ)
                                         Just (PolyTy ctx τ') -> do Right s <- runErrorT $ fitDeclTy τ τ'
                                                                    substCtx s ctx

simplifyCtx :: PolyCtx -> Typing PolyCtx
simplifyCtx [] = return []
simplifyCtx (π:πs) = do isRedundant <- or <$> mapM (π `isSuperOf`) πs
                        πs' <- filterM (`isNotSuperOf` π) πs
                        if isRedundant then simplifyCtx πs
                           else (π:) <$> simplifyCtx πs'
              where π `isNotSuperOf` π' = not <$> (π `isSuperOf` π')

isSuperOf :: PolyPred -> PolyPred -> Typing Bool
(cls, α) `isSuperOf` (cls', α') | α /= α'    = return False
                                | otherwise  = do supers <- askSupers cls'
                                                  return $ cls `elem` supers
                              
satisfies :: PolyCtx -> PolyCtx -> Typing Bool
general `satisfies` specific = and <$> mapM hasSuper specific
    where hasSuper π = or <$> mapM (π `isSuperOf`) general
                                                                             
