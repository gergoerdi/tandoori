module Sandbox where

import Tandoori.State    
import Tandoori.Ty.Printer
import Control.Monad.State
import Tandoori.Ty.PolyEnv
import Tandoori.Ty.Infer
import Tandoori.Test
import Language.Haskell.Syntax
import Language.Haskell.Pretty

test expr = niceTy $ snd $ evalState (infer emptyPoly expr) newState
                                     
main = do expr <- getTestExpr
          return $ test expr
