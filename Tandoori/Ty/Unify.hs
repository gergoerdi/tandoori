module Tandoori.Ty.Unify (Substitution, mgu, substTy) where

import Tandoori
import Tandoori.State    
import Tandoori.Ty
import Control.Monad.State
import Language.Haskell.Syntax
import Language.Haskell.Pretty
import qualified Data.Map as Map
import Data.Maybe
    
newtype Substitution = S (Map.Map TvName HsType) deriving Show

emptySubst :: Substitution
emptySubst = S $ Map.empty

getSubst :: Substitution -> TvName -> Maybe HsType
getSubst (S m) name = Map.lookup name m

addSubst :: Substitution -> TvName -> HsType -> Substitution
addSubst (S m) name ty = S $ Map.insert name ty m
                      
substTy :: Substitution -> HsType -> HsType
substTy s (HsTyFun t t') = HsTyFun (substTy s t) (substTy s t')
substTy s (HsTyApp t t') = HsTyApp (substTy s t) (substTy s t')
substTy s ty@(HsTyVar name) = case getSubst s name of
                                Nothing -> ty
                                Just ty' -> substTy s ty'
substTy s ty = ty
               
mgu :: [(HsType, HsType)] -> Either [(HsType, HsType)] Substitution
mgu [] = Right $ emptySubst
mgu ((HsTyVar x,      HsTyVar y)     :eqs) | x == y  = mgu eqs
mgu ((ty@(HsTyVar x), ty')           :eqs)           = if occurs x ty'
                                                       then combineErrors (ty, ty') (mgu eqs)
                                                       else case mgu eqs' of
                                                              Left errs -> Left errs
                                                              Right s   -> Right $ addSubst s x ty'
    where eqs' = map (\ (t, t') -> (subst t, subst t')) eqs
          subst = substTy (addSubst emptySubst x ty')
mgu ((ty,             tv@(HsTyVar _)):eqs)           = mgu $ (tv, ty):eqs
mgu ((HsTyCon c,      HsTyCon c')    :eqs) | c == c' = mgu eqs
mgu ((HsTyFun t u,    HsTyFun t' u') :eqs)           = mgu $ (t, t'):(u, u'):eqs
mgu ((HsTyApp t u,    HsTyApp t' u') :eqs)           = mgu $ (t, t'):(u, u'):eqs
mgu ((HsTyTuple ts,   HsTyTuple ts') :eqs)           = mgu $ (zip ts ts') ++ eqs
mgu ((l, r)                          :eqs)           = combineErrors (l, r) (mgu eqs)
                                                       
combineErrors :: (HsType, HsType) -> Either [(HsType, HsType)] Substitution -> Either [(HsType, HsType)] Substitution
combineErrors typair (Left errs) = Left $ typair:errs
combineErrors typair (Right s)   = Left $ [typair]
