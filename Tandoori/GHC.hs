module Tandoori.GHC (runDyn) where

import RdrHsSyn
import Outputable

import GHC
import GHC.Paths (libdir)
import DynFlags (defaultDynFlags)
import HscMain
    
runDyn m = defaultErrorHandler defaultDynFlags $ do
             runGhc (Just libdir) $ do
               --newSession
               dflags <- getSessionDynFlags
               setSessionDynFlags dflags
               -- env <- liftIO $ newHscEnv dflags
               -- setSession env
               m
