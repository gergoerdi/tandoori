import Tandoori.GHC    
import Tandoori.GHC.Parse
import Tandoori.GHC.Scope
import Tandoori.Kludge.Show

import Tandoori.State
import Control.Monad.State (evalState)
    
import GHC    
import RdrHsSyn
import Outputable
import IOEnv

import Tandoori.Ty.Infer
import Tandoori.Ty.PolyEnv
import Tandoori.Ty
    
src_filename = "input/list.hs"
               
typecheckMod mod = runDyn $ do
                     env <- getSession
                     (tydecls, group) <- liftIO $ runScope env mod
                     return $ evalState (inferValBinds p $ hs_valds group) mkState
    where p = emptyPoly

    
main = do mod <- parseMod src_filename
          typecheckMod mod
