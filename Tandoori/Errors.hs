{-# LANGUAGE ExistentialQuantification #-}

module Tandoori.Errors (ErrorLocation(..), ErrorSource(..), ErrorMessage(..), ErrorContent(..)) where

import Tandoori
import Tandoori.GHC.Internals    
import Tandoori.Ty.MonoEnv
import Tandoori.Ty.Pretty
    
import Data.Maybe

pshow x = showSDoc $ ppr x

data ErrorSource = forall src. Outputable src => ErrorSource src          
data ErrorLocation = ErrorLocation SrcSpan (Maybe ErrorSource)
data ErrorMessage = ErrorMessage ErrorLocation ErrorContent

instance Show ErrorMessage where
    show (ErrorMessage loc content) = unwords [showLoc loc, show content]
        where showLoc (ErrorLocation srcloc src) = showSrcLoc srcloc ++ ":"

              showSrc Nothing = ""
              showSrc (Just (ErrorSource src)) = showSDoc $ ppr src

              showSrcLoc srcloc = showSDoc $ ppr srcloc

instance Outputable ErrorMessage where
    ppr (ErrorMessage loc content) = pprLoc loc <+> text (show content)
        where pprLoc (ErrorLocation srcloc src) = ppr srcloc <> colon
                                  
data ErrorContent = OtherMessage String
                  | UndefinedCon ConName
                  | UndefinedVar VarName
                  | UnificationFailed [MonoEnv] [(TanType, TanType)]
                  | CantFitDecl TanType TanType [(TanType, TanType)]

showFailedEqs sep typairs = unwords $ map (\ (t1, t2) -> unwords [pshow t1, sep, pshow t2]) typairs
                    
instance Show ErrorContent where
    show (UndefinedCon name)             = unwords ["Reference to undefined constructor", showSDoc $ ppr name]
    show (UndefinedVar name)             = unwords ["Reference to undefined variable", showSDoc $ ppr name]
    show (UnificationFailed ms typairs)  = unwords ["Cannot unify", showFailedEqs "with" typairs'] -- ++  "\n" ++ (unwords $ map show ms)
        where typairs' = runPretty $ mapM prettyTyPairM typairs
              prettyTyPairM (t, u) = do t' <- prettyTyM t
                                        u' <- prettyTyM u
                                        return (t', u')                                             
    show (CantFitDecl tyDecl ty typairs) = unwords ["Declared type", pshow tyDecl', "is not a special case of inferred type", pshow ty']
        where (tyDecl', ty') = runPretty $ do tyDecl' <- prettyTyM tyDecl
                                              ty' <- prettyTyM ty
                                              return (tyDecl', ty')
    show (OtherMessage message)          = message
