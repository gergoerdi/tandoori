module Sandbox where

import Tandoori.State    
import Tandoori.Ty.Printer
import Control.Monad.State
import Tandoori.Ty.PolyEnv
import Tandoori.Ty.Infer
import Tandoori.Test
import Language.Haskell.Syntax
import Language.Haskell.Pretty

test expr = do (_, ty) <- infer emptyPoly expr                     
               errors <- getErrors
               return (ty, errors)
    
main' = do expr <- getTestExpr
           let (ty, errors) = evalState (test expr) newState
           mapM print errors
           return $ niceTy $ ty

main = liftM prettyPrint main'
