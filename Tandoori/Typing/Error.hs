module Tandoori.Typing.Error (ErrorLocation(..), ErrorMessage(..), TypingError(..), ErrorContent(..)) where

import Tandoori
import Tandoori.GHC.Internals
import Tandoori.Typing
import Tandoori.Typing.MonoEnv
import Control.Monad.Error

--- Errors    
data ErrorLocation = ErrorLocation SrcSpan (Maybe SDoc)
data ErrorMessage = ErrorMessage ErrorLocation ErrorContent

data TypingError = Unsolvable TyEq
                 | InfiniteType TyEq

instance Error TypingError where
    strMsg = undefined
                  
data ErrorContent = UndefinedCon ConName
                  | UndefinedVar VarName
                  | UndefinedCls Cls
                  | UnificationFailed [MonoEnv] TypingError
                  | CantFitDecl PolyTy PolyTy
                  | ClassCycle [Cls]
                  | InvalidClassCtx (Cls, Tv) PolyPred
                  | MissingBaseInstances OverPred PolyCtx
                  | InvalidInstance
                  | InvalidCon PolyTy
                  | AmbiguousPredicate PolyTy PolyPred
                  | UnfulfilledPredicate OverPred
                  | OtherError String

instance Error ErrorMessage where
    strMsg = ErrorMessage (ErrorLocation noSrcSpan Nothing) . OtherError
