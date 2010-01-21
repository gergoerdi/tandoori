module Tandoori.Ty.Unify (Substitution, substTy, mgu, fitDecl) where

import Tandoori
import Tandoori.State    
import Tandoori.Ty
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Tandoori.GHC.Internals

newtype Substitution = S (Map.Map TvName TanType)
    
emptySubst :: Substitution
emptySubst = S $ Map.empty

getSubst :: Substitution -> TvName -> Maybe TanType
getSubst (S m) name = Map.lookup name m

addSubst :: Substitution -> TvName -> TanType -> Substitution
addSubst (S m) name ty = S $ Map.insert name ty m

                         
lsubstTy :: Substitution -> (Located TanType) -> (Located TanType)
lsubstTy s = noLoc . substTy s . unLoc
                         
substTy :: Substitution -> TanType -> TanType
substTy s (HsFunTy lt lt')       = HsFunTy (lsubstTy s lt) (lsubstTy s lt')
substTy s (HsAppTy lt lt')       = HsAppTy (lsubstTy s lt) (lsubstTy s lt')
substTy s ty@(HsTyVar name)      = case getSubst s name of
                                     Nothing   -> ty
                                     Just ty'  -> substTy s ty'
substTy s (HsListTy lt)          = HsListTy $ lsubstTy s lt
substTy s (HsTupleTy b lts)      = HsTupleTy b $ map (lsubstTy s) lts
                                   
substTy s (HsParTy lty)          = HsParTy (lsubstTy s lty)
substTy s (HsDocTy lty ldoc)     = HsDocTy (lsubstTy s lty) ldoc
                                   
substTy s (HsBangTy _ _)         = error "substTy: TODO: Bang"
substTy s (HsForAllTy _ _ _ _)   = error "substTy: TODO: HsForAll"
substTy s (HsPArrTy _)           = error "substTy: TODO: PArrTy"
substTy s (HsKindSig _ _)        = error "substTy: TODO: KindSig"
substTy s (HsNumTy _)            = error "substTy: TODO: NumTy"
substTy s (HsOpTy _ _ _)         = error "substTy: TODO: OpTy"
substTy s (HsSpliceTy _)         = error "substTy: TODO: Splice"
substTy s (HsPredTy _ )          = error "substTy: TODO: Pred"

type UnsolvableEqs = [(TanType, TanType)]
    
fitDecl :: TanType -> TanType -> Either UnsolvableEqs Substitution
fitDecl tDecl t = mgu' True [(t, tDecl)]
                                                  
mgu :: [(TanType, TanType)] -> Either UnsolvableEqs Substitution
mgu = mgu' False

      
mgu' :: Bool -> [(TanType, TanType)] -> Either UnsolvableEqs Substitution
mgu' leftOnly []                                                                            = Right $ emptySubst
mgu' leftOnly ((HsParTy (L _ ty),         ty')                       :eqs)                  = mgu' leftOnly $ (ty, ty'):eqs
mgu' leftOnly ((ty,                       HsParTy (L _ ty'))         :eqs)                  = mgu' leftOnly $ (ty, ty'):eqs
                                                                                    
mgu' leftOnly ((HsTyVar x,                HsTyVar y)                 :eqs) | x == y         = mgu' leftOnly eqs
mgu' leftOnly ((ty,                       ty')                       :eqs) | isTyCon ty &&
                                                                             isTyCon ty'    = combineErrors (ty, ty') (mgu' leftOnly eqs)
mgu' leftOnly ((ty@(HsTyVar x),           ty')                       :eqs) | not(isTyCon ty)  = if occurs x ty'
                                                                                              then combineErrors (ty, ty') (mgu' leftOnly eqs)
                                                                                              else case mgu' leftOnly eqs' of
                                                                                                     Left errs -> Left errs
                                                                                                     Right s   -> Right $ addSubst s x ty'
    where eqs' = map (\ (t, t') -> (subst t, subst t')) eqs
          subst = substTy (addSubst emptySubst x ty')
mgu' leftOnly ((ty,                       tv@(HsTyVar _))            :eqs) | not leftOnly     = mgu' leftOnly $ (tv, ty):eqs
mgu' leftOnly ((HsFunTy (L _ t) (L _ u),  HsFunTy (L _ t') (L _ u')) :eqs)                  = mgu' leftOnly $ (t, t'):(u, u'):eqs
mgu' leftOnly ((HsAppTy (L _ t) (L _ u),  HsAppTy (L _ t') (L _ u')) :eqs)                  = mgu' leftOnly $ (t, t'):(u, u'):eqs
mgu' leftOnly ((HsTupleTy _ lts,          HsTupleTy _ lts')          :eqs)                  = mgu' leftOnly $ (zip (map unLoc lts) (map unLoc lts')) ++ eqs
mgu' leftOnly ((HsListTy (L _ t),         HsListTy (L _ t'))         :eqs)                  = mgu' leftOnly $ (t, t'):eqs

mgu' leftOnly ((HsBangTy _ (L _ t),       t')                        :eqs)                  = mgu' leftOnly $ (t, t'):eqs
mgu' leftOnly ((t,                        HsBangTy _ (L _ t'))       :eqs)                  = mgu' leftOnly $ (t, t'):eqs
                                                                                              
mgu' leftOnly ((HsForAllTy _ _ _ (L _ t), t')                        :eqs)                  = mgu' leftOnly $ (t, t'):eqs
mgu' leftOnly ((t,                        HsForAllTy _ _ _ (L _ t')) :eqs)                  = mgu' leftOnly $ (t, t'):eqs
                                                                                    
mgu' leftOnly ((t,                        t')                        :eqs)                  = combineErrors (t, t') (mgu' leftOnly eqs)

                                                                                              
combineErrors :: (TanType, TanType) -> Either UnsolvableEqs Substitution -> Either UnsolvableEqs Substitution
combineErrors typair (Left errs) = Left $ typair:errs
combineErrors typair (Right s)   = Left $ [typair]

                                   
