module Sandbox where

import Tandoori.State    
import Tandoori.Ty.Printer
import Language.Haskell.Syntax
import Control.Monad.State
import Tandoori.Ty.PolyEnv
import Tandoori.Ty.Infer
import Tandoori.Test

test expr = niceTy $ snd $ evalState (infer emptyPoly expr) newState
                                     
main = do expr <- getTestExpr
          return $ test expr
