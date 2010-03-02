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

instance Show Substitution where
    show = showSubst
                  
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

type TyEq = (TanType, TanType)
type TyEqProblem = (Bool, TyEq)
    
fitDecl :: TanType -> TanType -> Either [TyEq] UnificationRes
fitDecl tyDecl ty = mgu' True [(True, (ty, tyDecl))] -- TODO: Call ensurePredicates here
                                                  
mgu :: [TyEq] -> [TyEq] -> Either [TyEq] UnificationRes
mgu eqs eqsCollect = mgu' False $ (map (\ eq -> (False, eq)) eqs) ++ (map (\ eq -> (True, eq)) eqsCollect)

justSubst :: Substitution -> UnificationRes
justSubst s = (s, [])

addPred :: Bool -> UnificationRes -> [HsPred Name] -> UnificationRes 
addPred True   (s, preds)  preds'  = (s, preds ++ preds') 
addPred False  r           _       = r


mgu' :: Bool -> [TyEqProblem] -> Either [TyEq] UnificationRes
mgu' leftOnly []                                                                                             = Right $ emptySubst
mgu' leftOnly ((c, (HsParTy (L _ ty),               ty'))                             :eqs)                  = mgu' leftOnly $ (c, (ty, ty')):eqs
mgu' leftOnly ((c, (ty,                             HsParTy (L _ ty')))               :eqs)                  = mgu' leftOnly $ (c, (ty, ty')):eqs
                                                                                    
mgu' leftOnly ((c, (HsTyVar x,                      HsTyVar y))                       :eqs) | x == y         = mgu' leftOnly eqs
mgu' leftOnly ((c, (ty,                             ty'))                             :eqs) | isTyCon ty &&
                                                                                              isTyCon ty'    = combineErrors (ty, ty') (mgu' leftOnly eqs)
mgu' leftOnly ((c, (ty@(HsTyVar x),                 HsForAllTy _ _ lctxt (L _ ty')))  :eqs)                  = case mgu' leftOnly ((c, (ty, ty')):eqs) of
                                                                                                                 Left errs           -> Left errs
                                                                                                                 Right r@(s, preds)  -> Right $ addPred c r preds'
                                                                                                                     where preds' = map (unLoc . (substLPred s)) (unLoc lctxt)
mgu' leftOnly ((c, (ty@(HsTyVar x),                 ty'))                             :eqs) | not(isTyCon ty)  = if occurs x ty'
                                                                                                               then combineErrors (ty, ty') (mgu' leftOnly eqs)
                                                                                                               else case mgu' leftOnly eqs' of
                                                                                                                      Left errs -> Left errs
                                                                                                                      Right r   -> Right $ addSubst r x ty'
    where eqs' = map (\ (c, (t, t')) -> (c, (subst t, subst t'))) eqs
          subst = substTy (fst $ addSubst emptySubst x ty')
mgu' leftOnly ((c, (ty,                             ty'@(HsTyVar _)))                 :eqs) | not leftOnly     = mgu' leftOnly $ (c, (ty', ty)):eqs
mgu' leftOnly ((c, (HsFunTy (L _ ty) (L _ u),       HsFunTy (L _ ty') (L _ u')))      :eqs)                  = mgu' leftOnly $ (c, (ty, ty')):(c, (u, u')):eqs
mgu' leftOnly ((c, (HsAppTy (L _ ty) (L _ u),       HsAppTy (L _ ty') (L _ u')))      :eqs)                  = mgu' leftOnly $ (c, (ty, ty')):(c, (u, u')):eqs
mgu' leftOnly ((c, (HsTupleTy _ ltys,               HsTupleTy _ ltys'))               :eqs)                  = mgu' leftOnly $ (map (\ eq -> (c, eq)) eqs') ++ eqs
    where eqs' = zip (map unLoc ltys) (map unLoc ltys')
mgu' leftOnly ((c, (HsListTy (L _ ty),              HsListTy (L _ ty')))              :eqs)                  = mgu' leftOnly $ (c, (ty, ty')):eqs

mgu' leftOnly ((c, (HsBangTy _ (L _ ty),            ty'))                             :eqs)                  = mgu' leftOnly $ (c, (ty, ty')):eqs
mgu' leftOnly ((c, (ty,                             HsBangTy _ (L _ ty')))            :eqs)                  = mgu' leftOnly $ (c, (ty, ty')):eqs

mgu' leftOnly ((c, (HsForAllTy _ _ lctxt (L _ ty),  ty'))                             :eqs)                  = case mgu' leftOnly $ (c, (ty, ty')):eqs of
                                                                                                                 Left errs -> Left errs
                                                                                                                 Right r@(s, preds) -> Right $ addPred c r preds'
                                                                                                                     where preds' = map (unLoc . (substLPred s)) (unLoc lctxt)
mgu' leftOnly ((c, (ty,                             HsForAllTy _ _ lctxt (L _ ty')))  :eqs)                  = case mgu' leftOnly $ (c, (ty, ty')):eqs of
                                                                                                                 Left errs           -> Left errs
                                                                                                                 Right r@(s, preds)  -> Right $ addPred c r preds'
                                                                                                                     where preds' = map (unLoc . (substLPred s)) (unLoc lctxt)
mgu' leftOnly ((c, (ty,                             ty'))                             :eqs)                  = combineErrors (ty, ty') (mgu' leftOnly eqs)

                                                                                              
combineErrors :: (TanType, TanType) -> Either [TyEq] UnificationRes -> Either [TyEq] UnificationRes
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
