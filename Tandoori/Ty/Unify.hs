module Tandoori.Ty.Unify (Substitution, substTy, mgu) where

import Tandoori
import Tandoori.State    
import Tandoori.Ty
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe

import Tandoori.Kludge.Show
    
import HsTypes
import SrcLoc

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
substTy s (HsFunTy lt lt')  = HsFunTy (lsubstTy s lt) (lsubstTy s lt')
substTy s (HsAppTy lt lt')  = HsAppTy (lsubstTy s lt) (lsubstTy s lt')
substTy s ty@(HsTyVar name) = case getSubst s name of
                                Nothing -> ty
                                Just ty' -> substTy s ty'
substTy s (HsListTy lt)     = HsListTy $ lsubstTy s lt
substTy s (HsTupleTy b lts) = HsTupleTy b $ map (lsubstTy s) lts
substTy s (HsParTy lty)     = HsParTy (lsubstTy s lty)

substTy s (HsBangTy _ _)        = error "substTy: TODO: Bang"
substTy s (HsForAllTy _ _ _ _)  = error "substTy: TODO: HsForAll"
substTy s (HsPArrTy _)          = error "substTy: TODO: PArrTy"
substTy s (HsKindSig _ _)       = error "substTy: TODO: KindSig"
substTy s (HsNumTy _)           = error "substTy: TODO: NumTy"
substTy s (HsOpTy _ _ _)        = error "substTy: TODO: OpTy"
substTy s (HsSpliceTy _)        = error "substTy: TODO: Splice"
substTy s (HsPredTy _ )         = error "substTy: TODO: Pred"
substTy s ty                    = error $ show ty
                              
                              
mgu :: [(TanType, TanType)] -> Either [(TanType, TanType)] Substitution
mgu []                                                                            = Right $ emptySubst
mgu ((HsTyVar x,                HsTyVar y)                 :eqs) | x == y         = mgu eqs
mgu ((ty,                       ty')                       :eqs) | isTyCon ty && isTyCon ty' = combineErrors (ty, ty') (mgu eqs)
mgu ((ty@(HsTyVar x),           ty')                       :eqs) | not(isTyCon ty) = if occurs x ty'
                                                                                   then combineErrors (ty, ty') (mgu eqs)
                                                                                   else case mgu eqs' of
                                                                                          Left errs -> Left errs
                                                                                          Right s   -> Right $ addSubst s x ty'
    where eqs' = map (\ (t, t') -> (subst t, subst t')) eqs
          subst = substTy (addSubst emptySubst x ty')
mgu ((ty,                       tv@(HsTyVar _))            :eqs)                  = mgu $ (tv, ty):eqs
mgu ((HsFunTy (L _ t) (L _ u),  HsFunTy (L _ t') (L _ u')) :eqs)                  = mgu $ (t, t'):(u, u'):eqs
mgu ((HsAppTy (L _ t) (L _ u),  HsAppTy (L _ t') (L _ u')) :eqs)                  = mgu $ (t, t'):(u, u'):eqs
mgu ((HsTupleTy _ lts,          HsTupleTy _ lts')          :eqs)                  = mgu $ (zip (map unLoc lts) (map unLoc lts')) ++ eqs
mgu ((HsListTy (L _ t),         HsListTy (L _ t'))         :eqs)                  = mgu $ (t, t'):eqs

mgu ((HsForAllTy _ _ _ (L _ t), t')                        :eqs)                  = mgu $ (t, t'):eqs
mgu ((t,                        t'@(HsForAllTy _ _ _ _))   :eqs)                  = mgu $ (t', t):eqs
mgu ((HsBangTy _ (L _ t),       t')                        :eqs)                  = mgu $ (t, t'):eqs
mgu ((t,                        t'@(HsBangTy _ _))         :eqs)                  = mgu $ (t', t):eqs
                                                                                    
mgu ((t,                        t')                        :eqs)                  = combineErrors (t, t') $ mgu eqs
                                                       
combineErrors :: (TanType, TanType) -> Either [(TanType, TanType)] Substitution -> Either [(TanType, TanType)] Substitution
combineErrors typair (Left errs) = Left $ typair:errs
combineErrors typair (Right s)   = Left $ [typair]

                                   
