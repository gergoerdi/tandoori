module Tandoori.Errors (ErrorMessage(..), ErrorContent(..)) where

import Tandoori

import HsExpr
import SrcLoc
import Outputable

import Data.Maybe
import Tandoori.Ty.MonoEnv

pshow x = showSDoc $ ppr x
                       
data ErrorMessage name = ErrorMessage (Maybe SrcSpan) (Maybe (HsExpr name)) ErrorContent

instance (OutputableBndr name) => Show (ErrorMessage name) where
    show (ErrorMessage maybe_loc maybe_expr content) = concat [showLoc maybe_loc, ": ", showExpr maybe_expr, show content]
        where showLoc Nothing = "(no location)"
              showLoc (Just srcloc) = pshow srcloc
              showExpr Nothing = ""
              showExpr (Just expr) = concat ["\n", pshow expr, "\n"]
                  
data ErrorContent = OtherMessage String
                  | UndefinedCon ConName
                  | UndefinedVar VarName
                  | UnificationFailed [MonoEnv] [(TanType, TanType)]

instance Show ErrorContent where
    show (UndefinedCon name)            = unwords ["Reference to undefined constructor", showSDoc $ ppr name]
    show (UndefinedVar name)            = unwords ["Reference to undefined variable", showSDoc $ ppr name]
    show (UnificationFailed ms typairs) = (unwords ("Cannot unify" : map (\ (t1, t2) -> unwords [pshow t1, "with", pshow t2]) typairs)) -- ++  "\n" ++ (unwords $ map show ms)
    show (OtherMessage message)         = message
