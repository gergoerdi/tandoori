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
import Tandoori.Ty.DataType
    
src_filename = "input/datatype.hs"
               
typecheckMod mod = runDyn $ do
                     env <- getSession
                     (tydecls, group) <- liftIO $ runScope env mod
                     let cons = concat $ map constructorsFromDecl $ map unLoc tydecls
                         p = mkPoly cons
                     let infer = do
                              p' <- inferValBinds p $ hs_valds group
                              errors <- getErrors
                              return $ (p', errors)
                     return $ evalState infer mkState
    
main = do mod <- parseMod src_filename
          (p, errors) <- typecheckMod mod
          if not(null errors)
            then mapM print errors
            else return []
          return p
