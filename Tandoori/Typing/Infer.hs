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
import Tandoori.Typing.Instantiate
import Tandoori.Typing.DataType
import Tandoori.Typing.ClassDecl
import Tandoori.Typing.InstanceDecl
import Tandoori.Typing.Repr
    
import Tandoori.Typing.Show

import Control.Monad.Writer
import Control.Monad.Error
import Control.Applicative
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List

import Tandoori.GHC.Internals

import Bag (bagToList)
    
infer decls group = runTyping $ do
                      cons <- concat <$> mapM constructorsFromDecl decls
                      withCons cons $ do
                        cis <- classMap decls
                        withClasses cis $ do
                          insts <- mapM getInstance (hs_instds group)
                          withInstances insts $ do
                            (m, vars) <- inferValBinds (hs_valds group)
                            withPolyVars m vars $ do
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
                  ctx' <- resolvePred (cls', τ)
                  superOK <- ctx `satisfies` ctx'
                  unless superOK $ do
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
     -- return $ PolyTy [] tyInt -- Comment this out to have non-polymorph integer literals
                                                             
inferLExpr :: Located TanExpr -> Typing (MonoEnv, PolyTy)
inferLExpr lexpr = withLSrc lexpr $ inferExpr $ unLoc lexpr

inferExpr :: HsExpr Name -> Typing (MonoEnv, PolyTy)
inferExpr (HsLit lit) = noVars ⊢ (PolyTy [] $ typeOfLit lit)
inferExpr (HsOverLit overlit) = 
  do σ <- typeOfOverLit overlit
     noVars ⊢ σ
inferExpr (HsVar name) | isDataConName name = 
  do τ <- (instantiate =<< askCon name) `orRecover` mkTyVar
     noVars ⊢ PolyTy [] τ
inferExpr (ExplicitList _ lexprs) = 
  do (ms, σs) <- unzip <$> mapM inferLExpr lexprs
     (m, σ) <- unify ms σs
     m ⊢ ptyList σ
inferExpr (ExplicitTuple tupargs _ ) = 
  do (ms, σs) <- unzip <$> mapM inferTupArg tupargs
     (m, σ) <- unify ms [ptyTuple σs]
     m ⊢ σ
  where 
    inferTupArg (Present lexpr) = inferLExpr lexpr                          
inferExpr (HsApp ltyFun ltyParam) = 
  do (m1, σ1) <- inferLExpr ltyFun
     (m2, σ2) <- inferLExpr ltyParam
     α <- mkTyVar
     (m, σ) <- unify [m1, m2] [σ1, ptyCurryFun [σ2, PolyTy [] α]]
     case σ of
       (PolyTy ctx (TyFun τ3 τ4)) -> m ⊢ PolyTy ctx τ4
       _ -> 
         do -- TODO: Error reporting
            β <- mkTyVar
            m ⊢ PolyTy [] β                                                                 
inferExpr (HsLam (MatchGroup lmatches _)) = 
  do (ms, σs) <- unzip <$> mapM inferLMatch lmatches
     (m, σ) <- unify ms σs
     m ⊢ σ

-- TODO: move this to askPolyVar, kill askUserDecl
inferExpr (HsVar x) = 
  do decl <- askUserDecl x
     case decl of
       Just lσ  -> 
         do σ' <- instantiatePolyTy (unLoc lσ)
            noVars ⊢ σ'
       Nothing   -> 
         do pv <- askPolyVar x
            case pv of
              Nothing -> 
                do --monovars <- askForcedMonoVars
                   --unless (x `Set.member` monovars) $
                     --addError $ UndefinedVar x
                   α <- mkTyVar
                   x `typedAs` (PolyTy [] α)
              Just (m, σ) -> 
                do (m', σ') <- instantiateTyping (m, σ)
                   m' ⊢ σ'
                where tvsOfDef y = case getMonoVar m y of
                        Nothing -> Set.empty
                        Just (PolyTy _ τ) -> tvsOf τ
                             
inferExpr (HsLet binds lexpr) = 
  do (mBinds, vars) <- inferLocalBinds binds
     (m, σ) <- withPolyVars mBinds vars $ inferLExpr lexpr
     (m', σ') <- unify [m, mBinds] [σ]
     let isOutsideVisible var _ = not(var `Set.member` vars)
         m'' = filterMonoVars isOutsideVisible m'
     m'' ⊢ σ'
inferExpr (OpApp left op fixity right) = inferExpr $ HsApp (noLoc $ HsApp op left) right
inferExpr (HsIf cond thn els) = 
  do (mCond, σCond) <- inferLExpr cond
     (m1, σ1) <- inferLExpr thn
     (m2, σ2) <- inferLExpr els
     (mCond', _) <- unify [mCond] [σCond, PolyTy [] tyBool]
     (m, σ) <- unify [mCond', m1, m2] [σ1, σ2]
     m ⊢ σ
     
-- TODO:
inferExpr (NegApp expr negfun) = error "infer': TODO: NegApp"
inferExpr (HsPar lexpr) = inferLExpr lexpr

inferLocalBinds :: HsLocalBinds Name -> Typing (MonoEnv, VarSet)
inferLocalBinds (HsValBinds vb)      = inferValBinds vb
inferLocalBinds (HsIPBinds ipbinds)  = error "inferLocalBinds: HsIPBnds"
inferLocalBinds EmptyLocalBinds      = do return (noVars, Set.empty)

withLSigs lsigs f = 
  do decls <- catMaybes <$> mapM fromSig lsigs
     withUserDecls decls $ f
  where fromSig (L srcloc (TypeSig (L _ name) (L _ ty))) = 
          do τ <- fromHsType ty
             return $ Just $ (name, L srcloc τ)
        fromSig _ = return Nothing
    
inferValBinds :: HsValBinds Name -> Typing (MonoEnv, VarSet)
inferValBinds (ValBindsOut recbinds lsigs) =  withLSigs lsigs $ listenVars $ inferBindGroups $ map snd recbinds

inferBindGroups :: [LHsBinds Name] -> Typing MonoEnv
inferBindGroups [] = return noVars
inferBindGroups (b:bs) = 
  do (m, vars) <- listenVars $ inferBinds b
     m' <- withPolyVars m vars $ inferBindGroups bs
     (m'', _) <- unify [m, m'] []
     return m''

fromJust' (Nothing) = error "foo"
fromJust' (Just x) = x

checkCtxAmbiguity σ@(PolyTy ctx τ) = 
  do forM_ ctx $ \(cls, α) -> 
       unless (α `Set.member` tvs) $ raiseError $ AmbiguousPredicate σ (cls, α)
  where 
    tvs = tvsOf τ
                                                                           
checkDecl σDecl@(PolyTy ctxDecl τDecl) σ@(PolyTy ctx τ) = 
  do fit <- runErrorT $ fitDeclTy τDecl τ
     case fit of
       Left err -> raiseError $ CantFitDecl σDecl σ
       Right θ -> 
         do σ'@(PolyTy ctx' _) <- subst θ σ
            ctxOk <- ctxDecl `satisfies` ctx'
            unless ctxOk $ raiseError $ CantFitDecl σDecl σ'
          
withPolyVars :: MonoEnv -> VarSet -> Typing a -> Typing a         
withPolyVars m xs f =
  do ctxt <- toPolyCtxt m xs
     withCtxt ctxt f
         
toPolyCtxt :: MonoEnv -> VarSet -> Typing Ctxt
toPolyCtxt m xs = 
  do let varmap = map (\ x -> (x, fromJust' $ getMonoVar m x)) $ Set.toList xs
     -- TODO: Location
     polyvars <- catMaybes <$> mapM (toPoly m) varmap
     addReducedPolyVars polyvars
  where toPoly m (x, σ) = 
          do lookup <- askUserDecl x
             case lookup of
               Nothing -> 
                 do checkCtxAmbiguity σ
                    return $ Just (x, (m, σ))
               Just (L loc σDecl) -> 
                 doLoc loc $ 
                   do checkDecl σDecl σ
                      checkCtxAmbiguity σDecl
                      return Nothing

        addReducedPolyVars :: [(VarName, (MonoEnv, PolyTy))] -> Typing Ctxt
        addReducedPolyVars polyvars = 
          do ctxt <- askCtxt
             let polyvars' = map reduce polyvars
                 ctxt' = addPolyVars ctxt polyvars'
             return ctxt'                                     
          where reduce (x, (m, σ@(PolyTy _ τ))) = (x, (filterMonoVars hasOutsideVars m, σ))
                  where hasOutsideVars y (PolyTy _ τ') | y `Set.member` xs = False
                                                       | otherwise = not $ Set.null $ tvs `Set.intersection` tvsOf τ'
                        tvs = tvsOf τ

inferBinds :: LHsBinds Name -> Typing MonoEnv
inferBinds lbindbag = do let lbinds = bagToList lbindbag
                         ms <- mapM inferLBind lbinds
                         (m, _) <- unify ms []
                         return m
              
inferLBind :: (LHsBind Name) -> Typing MonoEnv
inferLBind lbind = withLSrc lbind $ inferBind $ unLoc lbind
                                                                                        
inferBind :: (HsBind Name) -> Typing MonoEnv
inferBind PatBind{pat_lhs = lpat, pat_rhs = grhss} = do ((m, σ), vars) <- listenVars $ inferLPat lpat -- TODO: add new vars to ctxt
                                                        (m', σ') <- withMonoVars vars $ inferGRhss grhss
                                                        (m'', _) <- unify [m, m'] [σ, σ']
                                                        return m''
inferBind VarBind{} = error "VarBind"
inferBind FunBind{fun_matches = MatchGroup lmatches _, fun_id = (L _ name)} = do tellVar name
                                                                                 -- TODO: why not introduce new monovars per group?
                                                                                 -- because each member of a group sends up its own idea of the others (in Δ), and they are unified later in the group level
                                                                                 (ms, σs) <- withMonoVars (Set.singleton name) $ stopVars $ 
                                                                                            unzip <$> mapM inferLMatch lmatches
                                                                                 (m, σ) <- unify ms σs
                                                                                 return $ addMonoVar m (name, σ)

inferLMatch :: (LMatch Name) -> Typing (MonoEnv, PolyTy)
inferLMatch lmatch = doLoc (getLoc lmatch) $ inferMatch $ unLoc lmatch
                               
inferMatch :: (Match Name) -> Typing (MonoEnv, PolyTy)
inferMatch (Match lpats _ grhss) = do ((ms, σs), vars) <- stopVars $ listenVars $ (unzip <$> mapM inferLPat lpats)
                                      (m, σ) <- withMonoVars vars $ inferGRhss grhss
                                      (m', σ') <- unify (m:ms) [ptyCurryFun (σs ++ [σ])]
                                      let m'' = filterMonoVars (\ v ty -> not (Set.member v vars)) m'
                                      m'' ⊢ σ'


inferGRhs (GRHS _ lexpr) = inferLExpr lexpr
inferGRhss (GRHSs lgrhss _) = do (ms, σs) <- unzip <$> mapM (inferGRhs . unLoc) lgrhss
                                 -- TODO: typecheck guards
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
                                                α <- mkTyVar
                                                name `typedAs` (PolyTy [] α)
inferPat (LitPat lit)                      = noVars ⊢ (PolyTy [] $ typeOfLit lit)
inferPat (ConPatIn (L _ con) details)  = do τCon <- askCon con -- TODO: errors
                                            (ms, σs) <- unzip <$> mapM inferLPat lpats
                                            α <- mkTyVar
                                            (m, σ) <- unify ms [PolyTy [] τCon, ptyCurryFun (σs ++ [PolyTy [] α])]
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
unify ms σs = 
  do eqs <- monoeqs
     α <- mkTyVar
     let eqs' = map (\ (PolyTy _ τ) -> (Nothing, α :=: τ)) σs
         σ = PolyTy (concatMap getCtx σs) α
     u <- runErrorT $ mgu $ eqs ++ eqs'
     case u of
       Left err -> raiseError $ UnificationFailed ms err
       Right θ -> 
         do ms' <- mapM (substMono θ) ms
            σ' <- subst θ σ
            return (combineMonos ms', σ')
         `orRecover` (return (combineMonos ms, PolyTy [] α))
                        
  where getCtx (PolyTy ctx _) = ctx
        monoeqs = 
          do tyvarmap <- Map.fromList <$> mapM mkVar varnames
             let mkEq (var, (PolyTy _ τ)) = (Just var, (fromJust $ Map.lookup var tyvarmap) :=: τ)
             return $ map mkEq vars
          where varnames = nub $ map fst vars
                mkVar var = 
                  do tv <- mkTyVar
                     return (var, tv)
        vars = concatMap getMonoVars ms
        substMono θ m = mapMonoM (subst θ) m
