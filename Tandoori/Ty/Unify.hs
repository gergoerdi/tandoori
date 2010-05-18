module Tandoori.Ty.Unify (Substitution, substCTy, mgu, fitDecl) where

import Tandoori
import Tandoori.Ty
import Tandoori.Ty.State    
import Tandoori.Ty.ShowTy    
import Tandoori.Errors
import Tandoori.GHC.Internals    
import Tandoori.Util
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
    
newtype Substitution = S (Map.Map TvName TanType)

showSubst :: Substitution -> String
showSubst (S s) = show s

instance Show Substitution where
    show = showSubst
                  
emptySubst :: Substitution
emptySubst = S Map.empty

getSubst :: Substitution -> TvName -> Maybe TanType
getSubst (S m) name = Map.lookup name m

addSubst :: Substitution -> TvName -> TanType -> Substitution
addSubst (S m) name ty = S $ Map.insert name ty m

data SubstRes a = SubstRes (Set.Set TvName) a

instance (Monad SubstRes) where
    (SubstRes tvs x) >>= f = SubstRes (tvs `Set.union` tvs') y
        where SubstRes tvs' y = f x
    return x = SubstRes Set.empty x
                
seenTyVar :: Name -> SubstRes ()
seenTyVar tv = SubstRes (Set.singleton tv) ()

getSubsted :: SubstRes a -> a
getSubsted (SubstRes tvs x) = x
                
substTy :: Substitution -> (HsType Name) -> SubstRes (HsType Name)
substTy s ty | isTyCon ty      = return ty
substTy s ty@(HsTyVar name)    = do seenTyVar name
                                    case getSubst s name of
                                      Nothing -> return ty
                                      Just ty' -> substTy s ty'
substTy s (HsFunTy ltyL ltyR)  = do ltyL' <- substLTy s ltyL
                                    ltyR' <- substLTy s ltyR
                                    return $ HsFunTy ltyL' ltyR'
substTy s (HsAppTy ltyL ltyR)  = do ltyL' <- substLTy s ltyL
                                    ltyR' <- substLTy s ltyR
                                    return $ HsFunTy ltyL' ltyR'
substTy s (HsListTy lty)       = liftM HsListTy $ substLTy s lty
substTy s (HsTupleTy b ltys)   = liftM (HsTupleTy b) $ mapM (substLTy s) ltys                                   
substTy s (HsParTy lty)        = liftM HsParTy $ substLTy s lty
                                           
                         
substLTy :: Substitution -> (Located (HsType Name)) -> SubstRes (Located (HsType Name))
substLTy s lty = do ty' <- substTy s (unLoc lty)
                    return $ noLoc ty'
                         
substCTy :: Substitution -> (CanonizedType, HsContext Name) -> CanonizedType
substCTy s (cty, ctxt) = mkCanonizedType ty' ctxtInEffect
    where SubstRes tvs ty' = substTy s ty
          -- ctxtInEffect = trace (show (ctxt, s)) $ (map (substLPred s) $ ctyLPreds cty) ++ (filter hasRelevantTyVars ctxt')
          ctxtInEffect = (map (substLPred s) $ ctyLPreds cty) ++ (filter hasRelevantTyVars ctxt')
          ctxt' = map (substLPred s) ctxt
          ty = ctyTy cty
          -- ctxt'' = map (substLPred s) ctxtInEffect
          -- hasRelevantTyVars lpred = True
          hasRelevantTyVars lpred = any (\ tv -> tv `Set.member` tvs) $ Set.toList $ tyVarsOfPred (unLoc lpred)
                                        
substLPred :: Substitution -> LHsPred Name -> LHsPred Name
substLPred s = noLoc . substPred s . unLoc
                                 
substPred :: Substitution -> HsPred Name -> HsPred Name
substPred s (HsClassP cls [lty]) = HsClassP cls [getSubsted $ substLTy s lty]

fitDecl :: TanType -> TanType -> Either [TyEq] Substitution
fitDecl tyDecl ty = mgu' True [(ty :=: tyDecl)] -- TODO: Call ensurePredicates here
                                                  
mgu :: [TyEq] -> [TyEq] -> Either [TyEq] Substitution
mgu eqs eqsCollect = mgu' False $ eqs ++ eqsCollect

simplifyTy :: HsType Name -> HsType Name
simplifyTy (HsParTy (L _ ty'))     = simplifyTy ty'
simplifyTy (HsBangTy _ (L _ ty'))  = simplifyTy ty'
simplifyTy ty                      = ty

data UnificationRes  = Substitute TvName (HsType Name)
                     | Skip
                     | Incongruent
                     | Flip UnificationRes
                     | Recurse [TyEq]
                     | OccursFailed
                                     
mguEq :: HsType Name -> HsType Name -> UnificationRes
mguEq (HsTyVar x)                (HsTyVar y)                 | x == y                 = Skip
mguEq t                          t'                          | isTyCon t && isTyCon t' = Incongruent
mguEq t@(HsTyVar x)              t'                          | not(isTyCon t)           = if occurs x t' then OccursFailed else Substitute x t'
mguEq t                          (HsTyVar _)                                          = Flip Incongruent
mguEq (HsFunTy (L _ t) (L _ u))  (HsFunTy (L _ t') (L _ u'))                          = Recurse [(t :=: t'), (u :=: u')]
mguEq (HsAppTy (L _ t) (L _ u))  (HsAppTy (L _ t') (L _ u'))                          = Recurse [(t :=: t'), (u :=: u')]
mguEq (HsTupleTy _ ltys)         (HsTupleTy _ ltys')                                  = Recurse (zipWith (:=:) tys tys')
    where tys = map unLoc ltys
          tys' = map unLoc ltys'
mguEq (HsListTy (L _ t))         (HsListTy (L _ t'))                                  = Recurse [(t :=: t')]
mguEq _                          _                                                    = Incongruent

mgu' :: Bool -> [TyEq] -> Either [TyEq] Substitution
mgu' leftOnly []               = Right emptySubst
mgu' leftOnly ((t :=: t'):eqs) = process $ mguEq t t'
    where process Skip              = mgu' leftOnly eqs
          process (Recurse eqs')    = mgu' leftOnly (eqs' ++ eqs)
          process Incongruent       = addError (t :=: t')
          process OccursFailed      = addError (t :=: t')
          process (Flip res)        = process $ if leftOnly then res else mguEq t' t
          process (Substitute x t)  = case mgu' leftOnly eqs' of
                                        Left errs -> Left errs
                                        Right s -> Right $ addSubst s x t
              where eqs' = map (\ (t :=: t') -> ((subst t) :=: (subst t'))) eqs
                        where s = addSubst emptySubst x t
                              subst t = getSubsted $ substTy s t
          addError err = case mgu' leftOnly eqs of
                           Left errs -> Left $ err:errs
                           Right _   -> Left $ [err]

explodePreds :: [TvName] -> [LHsPred Name] -> [(TvName, [LHsPred Name])]
explodePreds tvs lpreds = map explode tvs
    where explode tv = (tv, filter (occursLPred tv) lpreds)
          occursPred tv (HsClassP cls [lty]) = occurs tv (unLoc lty)
          occursLPred tv = occursPred tv . unLoc                           
