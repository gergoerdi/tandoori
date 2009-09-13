module Sandbox where

import Tandoori.State    
import Tandoori.Ty.Printer
import Control.Monad.State
import Tandoori.Ty.PolyEnv
import Tandoori.Ty.Infer
import Tandoori.Test
import Language.Haskell.Syntax
import Language.Haskell.Pretty
import Tandoori.Errors
    
test expr = do (_, ty) <- infer emptyPoly expr                     
               errors <- getErrors
               return (ty, errors)
    
main' = do expr <- getTestExpr
           let (ty, errors) = evalState (niceM $ evalState (test expr) newState) newTyPrinter
           mapM print errors
           return $ ty

    where niceM (ty, errors) = do errors' <- mapM niceErrorM errors
                                  ty' <- niceTyM ty
                                  return (ty', errors')
                  
niceErrorM (ErrorMessage loc expr content) = liftM (ErrorMessage loc expr) (niceContentM content)
niceContentM (UnificationFailed ms typairs) = do let ms' = ms
                                                 typairs' <- mapM niceTyPairM typairs
                                                 return $ UnificationFailed ms' typairs'
    where niceTyPairM (l, r) = do l' <- niceTyM l
                                  r' <- niceTyM r
                                  return (l', r')
niceContentM content = return $ content
                                              
                                             
main = liftM prettyPrint main'
