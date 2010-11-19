{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tandoori.Typing.Infer(infer) where

import Tandoori
import Tandoori.Typing
import Tandoori.Typing.Monad
import Tandoori.Typing.Error
import Tandoori.Typing.MonoEnv
import Tandoori.Typing.Ctxt
import Tandoori.Typing.Unify
import Tandoori.Typing.UnifyPred
import Tandoori.Typing.Substitute
import Tandoori.Typing.Instantiate
import Tandoori.Typing.DataType
import Tandoori.Typing.ClassDecl
import Tandoori.Typing.InstanceDecl
import Tandoori.Typing.Repr
    
import Control.Monad.Writer
import Control.Monad.Error
import Control.Applicative
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List

import Tandoori.GHC.Internals

import Bag (bagToList)
    
import Debug.Trace
import Tandoori.Typing.Show

infer decls group = runTyping $ do
                      cons <- concat <$> mapM constructorsFromDecl decls
                      withCons cons $ do
                        cis <- classMap decls
                        withClasses cis $ do
                          insts <- mapM getInstance (hs_instds group)
                          withInstances insts $ do
                            let valbinds@(ValBindsOut _ lsigs) = hs_valds group
                            (m, vars) <- inferValBinds valbinds
                            withLSigs lsigs $ withPolyVars False m vars $ do
                              mapM_ checkLInstance (hs_instds group)
                              askCtxt

    where getInstance linst = withLSrc linst $ instDecl (unLoc linst)
          checkLInstance linst = withLSrc linst $ checkInstance (unLoc linst)

          checkInstance inst@(InstDecl lty binds lsigs _)  = do
            ((cls, κ), σ) <- instDecl inst
            let PolyTy ctx τ = σ
            PolyTy ctx' τ' <- instantiatePolyTy σ
            ci <- askClass cls
            let checkSuper cls' = withLSrc lty $ do 
                  Right ctx' <- runErrorT $ resolvePred (cls', τ)
                  superOK <- ctx `satisfies` ctx'
                  unless superOK $
                    raiseError $ MissingBaseInstances (cls, τ) ctx'
                checkMember lbind = return () -- TODO
            mapM_ checkSuper (clsSupers ci)
            -- TODO: Typecheck instance functions
            (m, vars) <- stopVars $ listenVars $ withLSigs lsigs $ inferBinds binds
            return ()
                              
doLoc :: SrcSpan -> Typing a -> Typing a
doLoc srcloc m | isGoodSrcSpan srcloc  = withLoc srcloc $ m
               | otherwise             = m
           
typeOfOverLit :: HsOverLit Name -> Typing PolyTy
typeOfOverLit (OverLit { ol_val = HsIsString _ }) = return $ PolyTy [] tyString
typeOfOverLit (OverLit { ol_val = val }) = 
  do α <- mkTv
     let cls = case val of
           HsIntegral _ -> numClassName
     return $ PolyTy [(cls, α)] (TyVar α)
     return $ PolyTy [] tyInt -- Comment this out to have non-polymorph integer literals
                                                             
inferLExpr :: Located TanExpr -> Typing (MonoEnv, Ty)
inferLExpr lexpr = withLSrc lexpr $ inferExpr $ unLoc lexpr

inferExpr :: HsExpr Name -> Typing (MonoEnv, Ty)
inferExpr (HsLit lit) = noVars ⊢ typeOfLit lit
inferExpr (HsOverLit overlit) = 
  do PolyTy ctx τ <- typeOfOverLit overlit -- TODO: preserve ctx
     noVars ⊢ τ
inferExpr (HsVar name) | isDataConName name = 
  do τ <- (instantiate =<< askCon name) `orRecover` mkTyVar -- TODO: constructors with predicates
     noVars ⊢ τ
inferExpr (ExplicitList _ lexprs) = 
  do (ms, τs) <- unzip <$> mapM inferLExpr lexprs
     (m, τ) <- unify ms τs
     m ⊢ tyList τ
inferExpr (ExplicitTuple tupargs _ ) = 
  do (ms, τs) <- unzip <$> mapM inferTupArg tupargs
     (m, τ) <- unify ms [tyTuple τs]
     m ⊢ τ
  where 
    inferTupArg (Present lexpr) = inferLExpr lexpr                          
inferExpr (HsApp ltyFun ltyParam) = 
  do (m1, τ1) <- inferLExpr ltyFun
     (m2, τ2) <- inferLExpr ltyParam
     α <- mkTyVar
     (m, τ) <- unify [m1, m2] [τ1, tyCurryFun [τ2, α]]
     case τ of
       (TyFun τ3 τ4) -> m ⊢ τ4
       _ -> 
         do -- TODO: Error reporting
            β <- mkTyVar
            m ⊢ β                                                                 
inferExpr (HsLam (MatchGroup lmatches _)) = 
  do (ms, τs) <- unzip <$> mapM inferLMatch lmatches
     (m, τ) <- unify ms τs
     m ⊢ τ

-- TODO: move this to askPolyVar, kill askUserDecl
inferExpr (HsVar x) = 
  do decl <- askUserDecl x
     case decl of
       Just lσ -> 
         do σ' <- instantiatePolyTy (unLoc lσ)
            return $ justType σ'
       Nothing   -> 
         do pv <- askPolyVar x
            case pv of
              Nothing -> 
                do --monovars <- askForcedMonoVars
                   --unless (x `Set.member` monovars) $
                     --addError $ UndefinedVar x
                   α <- mkTyVar
                   x `typedAs` (PolyTy [] α)
              Just (m, τ) -> 
                do (m', τ') <- instantiateTyping (m, τ)
                   m' ⊢ τ'                                                
inferExpr (HsLet binds lexpr) = 
  do (mBinds, vars) <- inferLocalBinds binds
     ((m, τ), mBinds') <- withPolyVars False mBinds vars $ inferLExpr lexpr
     let mBinds'' = setMonoSrc mBinds' $ text "<definitions>"
     (m', τ') <- unify [m, mBinds''] [τ]
     let isOutsideVisible var _ = not(var `Set.member` vars)
         m'' = filterMonoVars isOutsideVisible m'
     m'' ⊢ τ'
inferExpr (OpApp left op fixity right) = inferExpr $ HsApp (noLoc $ HsApp op left) right
inferExpr (HsIf cond thn els) = 
  do (mCond, τCond) <- inferLExpr cond
     (m1, τ1) <- inferLExpr thn
     (m2, τ2) <- inferLExpr els
     (mCond', _) <- unify [mCond] [τCond, tyBool]
     (m, τ) <- unify [mCond', m1, m2] [τ1, τ2]
     m ⊢ τ
     
-- TODO:
inferExpr (NegApp expr negfun) = error "infer': TODO: NegApp"
inferExpr (HsPar lexpr) = inferLExpr lexpr

inferLocalBinds :: HsLocalBinds Name -> Typing (MonoEnv, VarSet)
inferLocalBinds (HsValBinds vb)      = inferValBinds vb
inferLocalBinds (HsIPBinds ipbinds)  = error "inferLocalBinds: HsIPBnds"
inferLocalBinds EmptyLocalBinds      = return (noVars, mempty)

withLSigs lsigs f = 
  do decls <- catMaybes <$> mapM fromSig lsigs
     withUserDecls decls $ f
  where fromSig (L srcloc (TypeSig (L _ name) (L _ ty))) = 
          do σ <- fromHsType ty
             return $ Just $ (name, L srcloc σ)
        fromSig _ = return Nothing
    
inferValBinds :: HsValBinds Name -> Typing (MonoEnv, VarSet)
inferValBinds (ValBindsOut recbinds lsigs) =  withLSigs lsigs $ listenVars $ inferBindGroups $ map snd recbinds

inferBindGroups :: [LHsBinds Name] -> Typing MonoEnv
inferBindGroups [] = return noVars
inferBindGroups (b:bs) = 
  do (m, vars) <- listenVars $ inferBinds b
     (mNext, m') <- withPolyVars True m vars $ inferBindGroups bs
     (m'', _) <- unify [mNext, m, m'] []
     return m''
     
checkMonoAmbiguity :: MonoEnv -> Ty -> Typing ()
checkMonoAmbiguity m τ = 
  do forM_ (getMonoPreds m) $ \(cls, α) ->
       unless (α `Set.member` tvs) $ 
         do -- trace (unwords [showName cls, showName α, show τ, show m]) $ return ()
            addError $ AmbiguousPredicate (Inferred (m, τ)) (cls, α)       
  where
    tvs = tvsOf τ `Set.union` (mconcat $ map (tvsOf . snd) $ getMonoVars m)

checkCtxAmbiguity σ@(PolyTy ctx τ) = 
  do forM_ ctx $ \(cls, α) -> 
       unless (α `Set.member` tvs) $ raiseError $ AmbiguousPredicate (Declared σ) (cls, α)
  where 
    tvs = tvsOf τ
                                                                           
checkDecl :: PolyTy -> (MonoEnv, Ty) -> Typing ()
checkDecl σDecl@(PolyTy ctxDecl τDecl) (m, τ) = 
  do fit <- runErrorT $ fitDeclTy τDecl τ
     case fit of
       Left err -> raiseError $ CantFitDecl σDecl (m, τ)
       Right θ -> 
         do let τ' = substTy θ τ
            Right m' <- runErrorT $ substMono θ m
            let ctx' = getMonoPreds m'
            -- trace (unlines [show m', show τ, show τ'])$ return ()
            ctxOk <- ctxDecl `satisfies` ctx'
            unless ctxOk $ raiseError $ CantFitDecl σDecl (m', τ')
          
withPolyVars :: Bool -> MonoEnv -> VarSet -> Typing a -> Typing (a, MonoEnv)
withPolyVars check m xs f =
  do --trace (unwords $ "withPolyVars":(map showName $ Set.toList xs)) $ return ()
     (ctxt, m) <- toPolyCtxt check m xs
     res <- withCtxt ctxt f
     return (res, m)
         
toPolyCtxt :: Bool -> MonoEnv -> VarSet -> Typing (Ctxt, MonoEnv)
toPolyCtxt check m xs = 
  do let m' = filterMonoVars (\ x _ -> x `Set.notMember` xs) m
         tvsOuter = mconcat $ map (tvsOf . snd) $ getMonoVars m'
         m'' = filterMonoPreds (\ (cls, α) -> α `Set.member` tvsOuter) m'
     -- TODO: Location
     polyvars <- catMaybes <$> (mapM (toPoly m) $ Set.toList xs)
     ctxt <- addReducedPolyVars polyvars
     return (ctxt, m'')
  where toPoly :: MonoEnv -> VarName -> Typing (Maybe (VarName, (MonoEnv, Ty)))
        toPoly m x = 
          do let τ = fromJust $ getMonoVar m x
             lookup <- askUserDecl x
             case lookup of
               Nothing -> 
                 do when check $ checkMonoAmbiguity m τ
                    return $ Just (x, (m, τ))
               Just (L loc σDecl) -> 
                 doLoc loc $ 
                   do when check $ 
                        do checkDecl σDecl (m, τ)
                           checkCtxAmbiguity σDecl
                      return Nothing

        addReducedPolyVars :: [(VarName, (MonoEnv, Ty))] -> Typing Ctxt
        addReducedPolyVars polyvars = 
          do ctxt <- askCtxt
             let polyvars' = map reduce polyvars
                 ctxt' = addPolyVars ctxt polyvars'
             return ctxt'                                     
          where reduce (x, (m, τ)) = (x, (m'', τ))
                  where tvs = tvsOf τ
                        hasOutsideVars y τ' | y `Set.member` xs = False
                                            | otherwise = not $ Set.null $ tvs `Set.intersection` tvsOf τ'           
                        m' = filterMonoVars hasOutsideVars m                        
                        tvs' = tvs `Set.union` (mconcat $ map (tvsOf . snd) $ getMonoVars m')
                        isOutsidePred (cls, α) = α `Set.member` tvs'
                        m'' = filterMonoPreds isOutsidePred m'

inferBinds :: LHsBinds Name -> Typing MonoEnv
inferBinds lbindbag = do let lbinds = bagToList lbindbag
                         ms <- mapM inferLBind lbinds                         
                         (m, _) <- unify ms []
                         return m
              
inferLBind :: (LHsBind Name) -> Typing MonoEnv
inferLBind lbind = withLSrc lbind $ inferBind $ unLoc lbind
                                                                                        
inferBind :: (HsBind Name) -> Typing MonoEnv
inferBind PatBind{pat_lhs = lpat, pat_rhs = grhss} = 
  do ((m, σ), vars) <- listenVars $ inferLPat lpat
     (m', σ') <- withMonoVars vars $ inferGRhss grhss
     (m'', _) <- unify [m, m'] [σ, σ']
     src <- askSrc -- TODO
     return $ maybe m'' (setMonoSrc m'') src
inferBind VarBind{} = error "VarBind"
inferBind FunBind{fun_matches = MatchGroup lmatches _, fun_id = (L _ f)} = 
  do tellVar f
     -- TODO: why not introduce new monovars per group?
     -- because each member of a group sends up its own idea of the others (in Δ), and they are unified later in the group level
     (ms, τs) <- withMonoVars (Set.singleton f) $ stopVars $ 
                  unzip <$> mapM inferLMatch lmatches
     (m, τ) <- unify ms τs     
     src <- askSrc
     let m' = maybe m (setMonoSrc m) src
     return $ addMonoVar m' (f, PolyTy [] τ)

inferLMatch :: (LMatch Name) -> Typing (MonoEnv, Ty)
inferLMatch lmatch = doLoc (getLoc lmatch) $ inferMatch $ unLoc lmatch
                               
inferMatch :: (Match Name) -> Typing (MonoEnv, Ty)
inferMatch (Match lpats _ grhss) = 
  do ((ms, τs), vars) <- stopVars $ listenVars $ (unzip <$> mapM inferLPat lpats)
     (m, τ) <- withMonoVars vars $ inferGRhss grhss
     (m', τ') <- unify (m:ms) [tyCurryFun (τs ++ [τ])]
     let m'' = filterMonoVars (\ x ty -> not (Set.member x vars)) m'
     m'' ⊢ τ'


inferGRhs (GRHS _ lexpr) = inferLExpr lexpr
inferGRhss (GRHSs lgrhss _) = 
  do (ms, σs) <- unzip <$> mapM (inferGRhs . unLoc) lgrhss
     -- TODO: typecheck guards
     (m, σ) <- unify ms σs
     m ⊢ σ

                                             
inferLPat :: Located (Pat Name) -> Typing (MonoEnv, Ty)
inferLPat lpat = withLSrc lpat $ inferPat $ unLoc lpat
                                     
inferPat :: Pat Name -> Typing (MonoEnv, Ty)
inferPat (AsPat (L _ name) lpat) = 
  do tellVar name
     (m, τ) <- inferLPat lpat                                
     let m' = addMonoVar m (name, PolyTy [] τ)
     m' ⊢ τ
inferPat (ParPat lpat) = inferLPat lpat
inferPat (WildPat _) = 
  do α <- mkTyVar
     noVars ⊢ α
inferPat (VarPat name) = 
  do tellVar name
     α <- mkTyVar
     name `typedAs` (PolyTy [] α)
inferPat (LitPat lit) = noVars ⊢ typeOfLit lit
inferPat (ConPatIn (L _ con) details)  = 
  do τCon <- askCon con -- TODO: errors, constructors with predicates
     (ms, τs) <- unzip <$> mapM inferLPat lpats
     α <- mkTyVar
     (m, τ) <- unify ms [τCon, tyCurryFun (τs ++ [α])]
     m ⊢ tyFunResult τ
  where lpats = 
          case details of
            PrefixCon lpats -> lpats
            InfixCon lp lp' -> [lp, lp']
        tyFunResult (TyFun τ1 τ2) = tyFunResult τ2
        tyFunResult τ = τ
inferPat (TuplePat lpats _ _) =              
  do (ms, τs) <- unzip <$> mapM inferLPat lpats
     combineMonos ms ⊢ tyTuple τs
inferPat (ListPat lpats _) =                
  do (ms, τs) <- unzip <$> mapM inferLPat lpats
     (m, τ) <- unify ms τs
     m ⊢ tyList τ
inferPat (NPat overlit _ _) =               
  do PolyTy ctx τ <- typeOfOverLit overlit
     noVars ⊢ τ -- TODO: preserve ctx
                                             
infix 1 ⊢
(⊢) :: MonoEnv -> Ty -> Typing (MonoEnv, Ty)
m ⊢ τ = do let m' = setMonoTy m τ
           src <- askSrc
           let m'' = case src of
                 Nothing -> m'
                 Just src -> setMonoSrc m' src
           -- trace (unwords ["    ", maybe "" showSDoc src, show m'', "|-", show τ]) $ return ()
           return (m'', τ)
                                             
typedAs :: VarName -> PolyTy -> Typing (MonoEnv, Ty)
x `typedAs` σ@(PolyTy ctx τ) = (addMonoVar noVars (x, σ)) ⊢ τ


unify :: [MonoEnv] -> [Ty] -> Typing (MonoEnv, Ty)
unify ms τs = 
  do α <- mkTyVar
     do eqs <- monoeqs     
        let eqs' = map (\ τ -> (Nothing, α :=: τ)) τs
        u <- runErrorT $ do
          θ <- mgu $ eqs ++ eqs'
          ms' <- mapM (substMono θ) ms
          let τ = substTy θ α
          return (combineMonos ms', τ)
        case u of
          Left err -> raiseError $ UnificationFailed ms err
          Right (ms', τ) -> return (ms', τ)
       `orRecover` return (combineMonos ms, α)
                        
  where getCtx (PolyTy ctx _) = ctx
        monoeqs = 
          do tyvarmap <- Map.fromList <$> mapM mkVar varnames
             let mkEq (var, τ) = (Just var, (fromJust $ Map.lookup var tyvarmap) :=: τ)
             return $ map mkEq vars
          where varnames = nub $ map fst vars
                mkVar var = 
                  do tv <- mkTyVar
                     return (var, tv)
        vars = concatMap getMonoVars ms
