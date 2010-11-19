module Tandoori.Typing.Error (ErrorLocation(..), ErrorMessage(..), TypeJudgement(..), TypingError(..), TypingErrorContent(..), ErrorContent(..)) where

import Tandoori
import Tandoori.GHC.Internals
import Tandoori.Typing
import Tandoori.Typing.MonoEnv
import Control.Monad.Error

--- Errors    
data ErrorLocation = ErrorLocation SrcSpan (Maybe SDoc)
data ErrorMessage = ErrorMessage ErrorLocation ErrorContent

data TypingErrorContent = Unsolvable TyEq
                        | InfiniteType TyEq
                        | UnfulfilledPredicate OverPred
data TypingError = TypingError { typingErrorSrc :: Maybe VarName,
                                 typingErrorContent :: TypingErrorContent }

instance Error TypingError where
    strMsg = undefined
                  
data TypeJudgement = Declared PolyTy             
                   | Inferred (MonoEnv, Ty)
             
data ErrorContent = UndefinedCon ConName
                  | UndefinedVar VarName
                  | UndefinedCls Cls
                  | UnificationFailed [MonoEnv] TypingError
                  | CantFitDecl PolyTy (MonoEnv, Ty)
                  | ClassCycle [Cls]
                  | InvalidClassCtx (Cls, Tv) PolyPred
                  | MissingBaseInstances OverPred PolyCtx
                  | InvalidInstance
                  | InvalidCon PolyTy
                  | AmbiguousPredicate TypeJudgement PolyPred
                  -- | UnfulfilledPredicate OverPred
                  | OtherError String

instance Error ErrorMessage where
    strMsg = ErrorMessage (ErrorLocation noSrcSpan Nothing) . OtherError
