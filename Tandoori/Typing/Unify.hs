module Tandoori.Typing.Unify (mgu, fitDeclTy) where

import Tandoori
import Tandoori.Typing
import Tandoori.Typing.Monad
import Tandoori.Typing.Error
import Control.Monad.Error
import Tandoori.Typing.Substitute
import Tandoori.Typing.Show
    
mgu :: [TyEq] -> ErrorT TypingError Typing Subst
mgu eqs = mgu' False eqs

fitDeclTy :: Ty -> Ty -> ErrorT TypingError Typing Subst
fitDeclTy τDecl τ = mgu' True [τ :=: τDecl]
    
data Unification  = Substitute Tv Ty
                  | Skip
                  | Incongruent
                  | Flip Unification
                  | Recurse [TyEq]
                  | OccursFailed
                    
mguEq :: TyEq -> ErrorT TypingError Typing Unification
mguEq (TyCon d   :=: TyCon d')                     = return $ if d == d' then Skip else Incongruent
mguEq (TyVar α   :=: TyVar α')       | α == α'     = return Skip
mguEq (TyVar α   :=: t')             | occurs α t' = return OccursFailed
                                     -- | otherwise   = do rigid <- isMonoTv α
                                     --                    return $ if rigid then Flip Rigid else Substitute α t'
                                     | otherwise   = return $ Substitute α t'
mguEq (t         :=: TyVar α)                      = return $ Flip Incongruent
mguEq (TyFun t u :=: TyFun t' u')                  = return $ Recurse [t :=: t', u :=: u']
mguEq (TyApp t u :=: TyApp t' u')                  = return $ Recurse [t :=: t', u :=: u']
mguEq (TyTuple n :=: TyTuple m)      | n == m      = return Skip
mguEq _                                            = return $ Incongruent

         
mgu' :: Bool -> [TyEq] -> ErrorT TypingError Typing Subst
mgu' leftOnly []               = return emptySubst
mgu' leftOnly ((t :=: t'):eqs) = process False =<< mguEq (t :=: t')
    where process flipped Skip              = mgu' leftOnly eqs
          process flipped (Recurse eqs')    = mgu' leftOnly (eqs' ++ eqs)
          process flipped Incongruent       = throwError $ Unsolvable (t :=: t')
          process flipped OccursFailed      = throwError $ InfiniteType (t :=: t')
          process flipped (Flip u)          = process True =<< if flipped || leftOnly then return u else mguEq (t' :=: t)
          process flipped (Substitute x t)  = do s <- mgu' leftOnly eqs'
                                                 return $ addSubst x t s
              where eqs' = map (\ (t :=: t') -> ((subst t) :=: (subst t'))) eqs
                        where s = addSubst x t emptySubst
                              subst t = substTy s t
