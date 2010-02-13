module Tandoori.Ty.Unify (Substitution, substTy, mgu, fitDecl) where

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
type UnificationRes = (Substitution, [HsPred Name])

showSubst :: Substitution -> String
showSubst (S s) = show s
    
emptySubst :: UnificationRes
emptySubst = (S Map.empty, [])

getSubst :: Substitution -> TvName -> Maybe TanType
getSubst (S m) name = Map.lookup name m

addSubst :: UnificationRes -> TvName -> TanType -> UnificationRes
addSubst ((S m), preds) name ty = (S $ Map.insert name ty m, preds)

                         
substLTy :: Substitution -> (Located TanType) -> (Located TanType)
substLTy s = noLoc . substTy s . unLoc
                         
substTy :: Substitution -> TanType -> TanType
substTy s (HsFunTy lt lt')            = HsFunTy (substLTy s lt) (substLTy s lt')
substTy s (HsAppTy lt lt')            = HsAppTy (substLTy s lt) (substLTy s lt')
substTy s ty@(HsTyVar name)           = case getSubst s name of
                                          Nothing   -> ty
                                          Just ty'  -> substTy s ty'
substTy s (HsListTy lt)               = HsListTy $ substLTy s lt
substTy s (HsTupleTy b lts)           = HsTupleTy b $ map (substLTy s) lts
                                   
substTy s (HsParTy lty)               = HsParTy (substLTy s lty)
substTy s (HsDocTy lty ldoc)          = HsDocTy (substLTy s lty) ldoc
                                   
substTy s (HsForAllTy e _ lctxt lty)  = HsForAllTy e noBinder lctxt' lty'
    where lctxt' = noLoc $ map (substLPred s) $ unLoc lctxt
          lty' = substLTy s lty
substTy s (HsBangTy _ _)              = error "substTy: TODO: Bang"
substTy s (HsPArrTy _)                = error "substTy: TODO: PArrTy"
substTy s (HsKindSig _ _)             = error "substTy: TODO: KindSig"
substTy s (HsNumTy _)                 = error "substTy: TODO: NumTy"
substTy s (HsOpTy _ _ _)              = error "substTy: TODO: OpTy"
substTy s (HsSpliceTy _)              = error "substTy: TODO: Splice"
substTy s (HsPredTy _ )               = error "substTy: TODO: Pred"

substLPred :: Substitution -> LHsPred Name -> LHsPred Name
substLPred s = noLoc . substPred s . unLoc
                                 
substPred :: Substitution -> HsPred Name -> HsPred Name
substPred s (HsClassP cls [lty]) = HsClassP cls [substLTy s lty]

type UnsolvableEqs = [(TanType, TanType)]
    
fitDecl :: TanType -> TanType -> Either UnsolvableEqs UnificationRes
fitDecl tyDecl ty = mgu' True [(ty, tyDecl)]
                                                  
mgu :: [(TanType, TanType)] -> Either UnsolvableEqs UnificationRes
mgu = mgu' False

justSubst :: Substitution -> UnificationRes
justSubst s = (s, [])

addPred :: UnificationRes -> [HsPred Name] -> UnificationRes 
addPred (s, preds) preds' = (s, preds ++ preds') 
              
mgu' :: Bool -> [(TanType, TanType)] -> Either UnsolvableEqs UnificationRes
mgu' leftOnly []                                                                                        = Right $ emptySubst
mgu' leftOnly ((HsParTy (L _ ty),               ty')                             :eqs)                  = mgu' leftOnly $ (ty, ty'):eqs
mgu' leftOnly ((ty,                             HsParTy (L _ ty'))               :eqs)                  = mgu' leftOnly $ (ty, ty'):eqs
                                                                                    
mgu' leftOnly ((HsTyVar x,                      HsTyVar y)                       :eqs) | x == y         = mgu' leftOnly eqs
mgu' leftOnly ((ty,                             ty')                             :eqs) | isTyCon ty &&
                                                                                         isTyCon ty'    = combineErrors (ty, ty') (mgu' leftOnly eqs)
mgu' leftOnly ((ty@(HsTyVar x),                 HsForAllTy _ _ lctxt (L _ ty'))  :eqs)                  = case mgu' leftOnly ((ty, ty'):eqs) of
                                                                                                            Left errs           -> Left errs
                                                                                                            Right r@(s, preds)  -> Right $ addPred r preds'
                                                                                                                where preds' = map (unLoc . (substLPred s)) (unLoc lctxt)
mgu' leftOnly ((ty@(HsTyVar x),                 ty')                             :eqs) | not(isTyCon ty)  = if occurs x ty'
                                                                                                          --then combineErrors (ty, ty') (mgu' leftOnly eqs)
                                                                                                          then error $ unwords ["occurs", show x, show ty']
                                                                                                          else case mgu' leftOnly eqs' of
                                                                                                                 Left errs -> Left errs
                                                                                                                 Right r   -> Right $ addSubst r x ty'
    where eqs' = map (\ (t, t') -> (subst t, subst t')) eqs
          subst = substTy (fst $ addSubst emptySubst x ty')
mgu' leftOnly ((ty,                             ty'@(HsTyVar _))                 :eqs) | not leftOnly     = mgu' leftOnly $ (ty', ty):eqs
mgu' leftOnly ((HsFunTy (L _ ty) (L _ u),       HsFunTy (L _ ty') (L _ u'))      :eqs)                  = mgu' leftOnly $ (ty, ty'):(u, u'):eqs
mgu' leftOnly ((HsAppTy (L _ ty) (L _ u),       HsAppTy (L _ ty') (L _ u'))      :eqs)                  = mgu' leftOnly $ (ty, ty'):(u, u'):eqs
mgu' leftOnly ((HsTupleTy _ ltys,               HsTupleTy _ ltys')               :eqs)                  = mgu' leftOnly $ (zip (map unLoc ltys) (map unLoc ltys')) ++ eqs
mgu' leftOnly ((HsListTy (L _ ty),              HsListTy (L _ ty'))              :eqs)                  = mgu' leftOnly $ (ty, ty'):eqs

mgu' leftOnly ((HsBangTy _ (L _ ty),            ty')                             :eqs)                  = mgu' leftOnly $ (ty, ty'):eqs
mgu' leftOnly ((ty,                             HsBangTy _ (L _ ty'))            :eqs)                  = mgu' leftOnly $ (ty, ty'):eqs

-- mgu' leftOnly ts                                                                                        = error $ show ts
mgu' leftOnly ((HsForAllTy _ _ lctxt (L _ ty),  ty')                             :eqs)                  = case mgu' leftOnly $ (ty, ty'):eqs of
                                                                                                           Left errs -> Left errs
                                                                                                           Right (s, preds) -> Right (s, preds' ++ preds)
                                                                                                               where preds' = map (unLoc . (substLPred s)) (unLoc lctxt)
mgu' leftOnly ((ty,                             HsForAllTy _ _ lctxt (L _ ty'))  :eqs)                  = case mgu' leftOnly $ (ty, ty'):eqs of
                                                                                                           Left errs           -> Left errs
                                                                                                           Right r@(s, preds)  -> Right $ addPred r preds'
                                                                                                               where preds' = map (unLoc . (substLPred s)) (unLoc lctxt)
mgu' leftOnly ((ty,                             ty')                             :eqs)                  = combineErrors (ty, ty') (mgu' leftOnly eqs)

                                                                                              
combineErrors :: (TanType, TanType) -> Either UnsolvableEqs UnificationRes -> Either UnsolvableEqs UnificationRes
combineErrors typair (Left errs) = Left $ typair:errs
combineErrors typair (Right _)   = Left $ [typair]

                                   
explodePreds :: [TvName] -> [LHsPred Name] -> [(TvName, [LHsPred Name])]
explodePreds tvs lpreds = map explode tvs
    where explode tv = (tv, filter (occursLPred tv) lpreds)
          occursPred tv (HsClassP cls [lty]) = occurs tv (unLoc lty)
          occursLPred tv = occursPred tv . unLoc                           
