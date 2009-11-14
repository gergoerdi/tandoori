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
                  | CantFitDecl TanType TanType [(TanType, TanType)]

showFailedEqs sep typairs = unwords $ map (\ (t1, t2) -> unwords [pshow t1, sep, pshow t2]) typairs
                    
instance Show ErrorContent where
    show (UndefinedCon name)            = unwords ["Reference to undefined constructor", showSDoc $ ppr name]
    show (UndefinedVar name)            = unwords ["Reference to undefined variable", showSDoc $ ppr name]
    show (UnificationFailed ms typairs) = unwords ["Cannot unify", showFailedEqs "with" typairs] -- ++  "\n" ++ (unwords $ map show ms)
    show (CantFitDecl tDecl t typairs)  = unwords ["Declared type", pshow tDecl, "is not a special case of inferred type", pshow t]
    show (OtherMessage message)         = message
