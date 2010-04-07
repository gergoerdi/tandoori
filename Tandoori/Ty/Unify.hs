module Tandoori.Ty.Unify (Substitution, substCTy, mgu, fitDecl) where

import Tandoori
import Tandoori.Util
import Tandoori.State    
import Tandoori.Ty
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Tandoori.GHC.Internals

import Tandoori.Ty.ShowTy    
import Tandoori.Kludge.Show    
    
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

type TyEq = (TanType, TanType)
type TyEqProblem = (Bool, TyEq)
    
fitDecl :: TanType -> TanType -> Either [TyEq] Substitution
fitDecl tyDecl ty = mgu' True [(ty, tyDecl)] -- TODO: Call ensurePredicates here
                                                  
mgu :: [TyEq] -> [TyEq] -> Either [TyEq] Substitution
mgu eqs eqsCollect = mgu' False $ eqs ++ eqsCollect

mgu' :: Bool -> [TyEq] -> Either [TyEq] Substitution
mgu' leftOnly []                                                                                        = Right $ emptySubst
mgu' leftOnly ((HsParTy (L _ ty),               ty')                             :eqs)                  = mgu' leftOnly $ (ty, ty'):eqs
mgu' leftOnly ((ty,                             HsParTy (L _ ty'))               :eqs)                  = mgu' leftOnly $ (ty, ty'):eqs
                                                                                    
mgu' leftOnly ((HsTyVar x,                      HsTyVar y)                       :eqs) | x == y         = mgu' leftOnly eqs
mgu' leftOnly ((ty,                             ty')                             :eqs) | isTyCon ty &&
                                                                                         isTyCon ty'    = combineErrors (ty, ty') (mgu' leftOnly eqs)
mgu' leftOnly ((ty@(HsTyVar x),                 ty')                             :eqs) | not(isTyCon ty)  = if occurs x ty'
                                                                                                          then combineErrors (ty, ty') (mgu' leftOnly eqs)
                                                                                                          else case mgu' leftOnly eqs' of
                                                                                                                 Left errs -> Left errs
                                                                                                                 Right r   -> Right $ addSubst r x ty'
    where eqs' = map (\ (t, t') -> (subst t, subst t')) eqs
          subst t = getSubsted $ substTy (addSubst emptySubst x ty') t
mgu' leftOnly ((ty,                             ty'@(HsTyVar _))                 :eqs) | not leftOnly     = mgu' leftOnly $ (ty', ty):eqs
mgu' leftOnly ((HsFunTy (L _ ty) (L _ u),       HsFunTy (L _ ty') (L _ u'))      :eqs)                  = mgu' leftOnly $ (ty, ty'):(u, u'):eqs
mgu' leftOnly ((HsAppTy (L _ ty) (L _ u),       HsAppTy (L _ ty') (L _ u'))      :eqs)                  = mgu' leftOnly $ (ty, ty'):(u, u'):eqs
mgu' leftOnly ((HsTupleTy _ ltys,               HsTupleTy _ ltys')               :eqs)                  = mgu' leftOnly $ eqs' ++ eqs
    where eqs' = zip (map unLoc ltys) (map unLoc ltys')
mgu' leftOnly ((HsListTy (L _ ty),              HsListTy (L _ ty'))              :eqs)                  = mgu' leftOnly $ (ty, ty'):eqs

mgu' leftOnly ((HsBangTy _ (L _ ty),            ty')                             :eqs)                  = mgu' leftOnly $ (ty, ty'):eqs
mgu' leftOnly ((ty,                             HsBangTy _ (L _ ty'))            :eqs)                  = mgu' leftOnly $ (ty, ty'):eqs
mgu' leftOnly ((ty,                             ty')                             :eqs)                  = combineErrors (ty, ty') (mgu' leftOnly eqs)

                                                                                              
combineErrors :: (TanType, TanType) -> Either [TyEq] Substitution -> Either [TyEq] Substitution
combineErrors typair (Left errs) = Left $ typair:errs
combineErrors typair (Right _)   = Left $ [typair]

                                   
explodePreds :: [TvName] -> [LHsPred Name] -> [(TvName, [LHsPred Name])]
explodePreds tvs lpreds = map explode tvs
    where explode tv = (tv, filter (occursLPred tv) lpreds)
          occursPred tv (HsClassP cls [lty]) = occurs tv (unLoc lty)
          occursLPred tv = occursPred tv . unLoc                           

-- testMgu = do tv <- mkTv
--              let ty1 = tyCurryFun [tv, tv]
--                  ty2 = HsForAllTy undefined undefined lctxt (noLoc ty1)
--                  lctxt = noLoc [noLoc $ HsClassP numClassName [noLoc tv]]
--              return $ (ty1, ty2, mgu [(ty1, ty2)])
