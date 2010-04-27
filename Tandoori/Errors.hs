{-# LANGUAGE ExistentialQuantification #-}

module Tandoori.Errors (ErrorLocation(..), ErrorSource(..), ErrorMessage(..), ErrorContent(..), TyEq(..)) where

import Tandoori
import Tandoori.Ty
import Tandoori.Ty.Canonize
import Tandoori.GHC.Internals    
import Tandoori.Ty.MonoEnv
import Tandoori.Ty.Pretty
import Tandoori.Ty.ShowTy
    
import Data.Maybe

data TyEq = HsType Name :=: HsType Name

instance (Show TyEq) where
    show (t :=: t') = unwords [(showTy t), ":=:", showTy t']
    
data ErrorSource = forall src. Outputable src => ErrorSource src                                  
data ErrorLocation = ErrorLocation SrcSpan (Maybe ErrorSource)
data ErrorMessage = ErrorMessage ErrorLocation ErrorContent

instance Outputable ErrorSource where
    ppr (ErrorSource src) = ppr src
                  
instance Show ErrorMessage where
    show (ErrorMessage loc content) = unwords [showLoc loc, show content]
        where showLoc (ErrorLocation srcloc src) = showSrcLoc srcloc ++ ":"
              showSrcLoc srcloc = showSDoc $ ppr srcloc

instance Outputable ErrorMessage where
    ppr (ErrorMessage (ErrorLocation srcloc Nothing) content)     = ppr srcloc <> colon <+> ppr content
    ppr (ErrorMessage (ErrorLocation srcloc (Just src)) content)  = ppr srcloc <> colon $$ ppr src $$ ppr content
                                  
data ErrorContent = OtherMessage String
                  | UndefinedCon ConName
                  | UndefinedVar VarName
                  | UnificationFailed [MonoEnv] [TyEq]
                  | CantFitDecl CanonizedType CanonizedType [TyEq]
                  | AmbiguousPredicate (HsType Name) (HsPred Name)

showFailedEqs sep tyeqs = unwords $ map (\ (t1 :=: t2) -> unwords [show t1, sep, show t2]) tyeqs

instance Outputable ErrorContent where
    ppr (UndefinedCon name)              = text "Reference to undefined constructor" <+> ppr name
    ppr (UndefinedVar name)              = text "Reference to undefined variable" <+> ppr name
    ppr (UnificationFailed ms tyeqs)     = text "Cannot unify" <+> text (showFailedEqs "with" tyeqs')
        where tyeqs' = runPretty $ mapM prettyTyEqM tyeqs
    ppr (CantFitDecl tyDecl ty tyeqs)    = text "Declared type" <+> text (show tyDecl') <+> text "is not a special case of inferred type" <+> text (show ty')
        where (tyDecl', ty') = runPretty $ do tyDecl' <- prettyTyM $ uncanonize tyDecl
                                              ty' <- prettyTyM $ uncanonize ty
                                              return (tyDecl', ty')
    ppr (AmbiguousPredicate ty pred)     = text "Ambiguous predicate" <+> text (showPred pred') <+> text "for type" <+> text (show ty')
        where (ty', pred') = runPretty $ do ty' <- prettyTyM ty
                                            tyPred' <- prettyTyM tyPred
                                            return (ty', HsClassP cls [L loc tyPred'])
                  where HsClassP cls [ltyPred] = pred
                        L loc tyPred = ltyPred
    ppr (OtherMessage message)           = text message

instance Show ErrorContent where
    show (UndefinedCon name)             = unwords ["Reference to undefined constructor", showSDoc $ ppr name]
    show (UndefinedVar name)             = unwords ["Reference to undefined variable", showSDoc $ ppr name]
    show (UnificationFailed ms tyeqs)    = unwords ["Cannot unify", showFailedEqs "with" typairs'] -- ++  "\n" ++ (unwords $ map show ms)
        where typairs' = runPretty $ mapM prettyTyEqM tyeqs
    show (CantFitDecl tyDecl ty typairs) = unwords ["Declared type", show tyDecl', "is not a special case of inferred type", show ty']
        where (tyDecl', ty') = runPretty $ do tyDecl' <- prettyTyM $ uncanonize tyDecl
                                              ty' <- prettyTyM $ uncanonize ty
                                              return (tyDecl', ty')
    show (OtherMessage message)          = message

prettyTyEqM (t :=: u) = do t' <- prettyTyM t
                           u' <- prettyTyM u
                           return $ t' :=: u'
