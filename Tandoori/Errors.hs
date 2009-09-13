module Tandoori.Errors (ErrorMessage(..), ErrorContent(..)) where

import Tandoori    
import Language.Haskell.Syntax
import Data.Maybe
import Language.Haskell.Pretty
import Tandoori.Ty.MonoEnv
    
data ErrorMessage = ErrorMessage (Maybe SrcLoc) (Maybe HsExp) ErrorContent
                             
instance Show ErrorMessage where
    show (ErrorMessage maybe_loc maybe_expr content) = concat [showLoc maybe_loc, ": ", showExpr maybe_expr, show content]
        where showLoc Nothing = "(no location)"
              showLoc (Just SrcLoc {srcFilename = filename, srcLine = line, srcColumn = col}) = concat [filename, ":", show line, ":", show col]
              showExpr Nothing = ""
              showExpr (Just expr) = concat ["\n", prettyPrint expr, "\n"]

                  
data ErrorContent = OtherMessage String
                  | UndefinedCon ConName
                  | UndefinedVar VarName
                  | UnificationFailed [MonoEnv] [(HsType, HsType)]

instance Show ErrorContent where
    show (UndefinedCon name)            = unwords ["Reference to undefined constructor", prettyPrint name]
    show (UndefinedVar name)            = unwords ["Reference to undefined variable", prettyPrint name]
    show (UnificationFailed ms typairs) = unwords $ "Cannot unify" : map (\ (t1, t2) -> unwords [prettyPrint t1, "with", prettyPrint t2]) typairs
    show (OtherMessage message)         = message
