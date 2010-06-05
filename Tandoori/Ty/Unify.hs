module Tandoori.Ty.Unify (mgu, fitDecl) where

import Tandoori
import Tandoori.Ty
import Tandoori.Ty.Substitute
import Tandoori.Errors
import Tandoori.GHC.Internals    
    
fitDecl :: TanType -> TanType -> Either [TyEq] Substitution
fitDecl tyDecl ty = mgu' True [(ty :=: tyDecl)] -- TODO: Call ensurePredicates here
                                                  
mgu :: [TyEq] -> Either [TyEq] Substitution
mgu eqs = mgu' False eqs

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
                              subst t = substTy s t
          addError err = case mgu' leftOnly eqs of
                           Left errs -> Left $ err:errs
                           Right _   -> Left $ [err]

explodePreds :: [TvName] -> [LHsPred Name] -> [(TvName, [LHsPred Name])]
explodePreds tvs lpreds = map explode tvs
    where explode tv = (tv, filter (occursLPred tv) lpreds)
          occursPred tv (HsClassP cls [lty]) = occurs tv (unLoc lty)
          occursLPred tv = occursPred tv . unLoc                           
