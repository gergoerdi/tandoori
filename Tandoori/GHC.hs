module Tandoori.GHC (runDyn) where

import GHC (runGhc, getSessionDynFlags, setSessionDynFlags, defaultErrorHandler)
import GHC.Paths (libdir)
import DynFlags (defaultDynFlags)
    
runDyn m = defaultErrorHandler defaultDynFlags $ do
             runGhc (Just libdir) $ do
               --newSession
               dflags <- getSessionDynFlags
               setSessionDynFlags dflags
               -- env <- liftIO $ newHscEnv dflags
               -- setSession env
               m
