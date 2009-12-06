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

import System.Environment    
    
import Tandoori.Ty.Infer
import Tandoori.Ty.PolyEnv
import Tandoori.Ty
import Tandoori.Ty.DataType
import Tandoori.Ty.ShowTy

import qualified Data.Map as Map
    
import IPPrint
    
src_filename = "input/declare.hs"

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
    
main' [src_filename] = do mod <- parseMod src_filename
                          (p, errors) <- typecheckMod mod
                          if not(null errors)
                            then mapM_ (\ error -> printErrs $ ppr error defaultUserStyle) errors
                            else return ()
                          printPolyEnv p
                          return p

main' _ = error "Usage: tandoori filename.hs" 

main = do args <- getArgs
          main' args

test = do p <- main' ["input/cikk.hs"]
          let tyFoo = snd $ snd $ (Map.toList $ polyvarmap p)!!0
              ltyId = snd $ (Map.toList $ userdecls p)!!1
              tyId = unLoc ltyId
              HsTyVar nGen = tyFoo
              HsForAllTy _ _ _ (L _ (HsFunTy _ (L _ (HsTyVar nUser)))) = tyId
          return (tyFoo, tyId)
