module Tandoori.Typing.Unify (mgu, fitDeclTy) where

import Tandoori
import Tandoori.Typing
import Tandoori.Typing.Monad
import Tandoori.Typing.Error
import Control.Monad.Error
import Tandoori.Typing.Substitute
    
mgu :: [(Maybe VarName, TyEq)] -> ErrorT TypingError Typing Subst
mgu eqs = mgu' False eqs

fitDeclTy :: Ty -> Ty -> ErrorT TypingError Typing Subst
fitDeclTy τDecl τ = mgu' True [(Nothing, τ :=: τDecl)]
    
data Unification  = Skip
                  | Substitute Tv Ty
                  | Recurse [TyEq]
                  | Flip Unification
                  | Incongruent
                  | OccursFailed
                    
mguEq :: TyEq -> ErrorT TypingError Typing Unification
mguEq (TyCon d   :=: TyCon d')                     = return $ if d == d' then Skip else Incongruent
mguEq (TyVar α   :=: TyVar α')       | α == α'     = return Skip
mguEq (TyVar α   :=: τ')             | occurs α τ' = return OccursFailed
                                     -- | otherwise   = do rigid <- isMonoTv α
                                     --                    return $ if rigid then Flip Rigid else Substitute α t'
                                     | otherwise   = return $ Substitute α τ'
mguEq (τ         :=: TyVar α)                      = return $ Flip Incongruent
mguEq (TyFun τ μ :=: TyFun τ' μ')                  = return $ Recurse [τ :=: τ', μ :=: μ']
mguEq (TyApp τ μ :=: TyApp τ' μ')                  = return $ Recurse [τ :=: τ', μ :=: μ']
mguEq (TyTuple n :=: TyTuple m)      | n == m      = return Skip
mguEq _                                            = return $ Incongruent

         
mgu' :: Bool -> [(Maybe VarName, TyEq)] -> ErrorT TypingError Typing Subst
mgu' leftOnly []                    = return emptySubst
mgu' leftOnly ((src, t :=: t'):eqs) = process False =<< mguEq (t :=: t')
    where process flipped Skip              = mgu' leftOnly eqs
          process flipped (Recurse eqs')    = mgu' leftOnly (map (\ eq -> (src, eq)) eqs' ++ eqs)
          process flipped Incongruent       = throwError $ TypingError src $ Unsolvable (t :=: t')
          process flipped OccursFailed      = throwError $ TypingError src $ InfiniteType (t :=: t')
          process flipped (Flip u)          = process True =<< if flipped || leftOnly then return u else mguEq (t' :=: t)
          process flipped (Substitute x t)  = do s <- mgu' leftOnly eqs'
                                                 return $ addSubst x t s
              where eqs' = map (fmap (\ (t :=: t') -> ((subst t) :=: (subst t')))) eqs
                        where s = addSubst x t emptySubst
                              subst t = substTy s t
