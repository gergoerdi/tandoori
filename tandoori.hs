module Main where

import Tandoori.GHC    
import Tandoori.GHC.Parse
import Tandoori.GHC.Scope

import GHC    
import Outputable
import IOEnv

import System.Environment    
    
import Tandoori.Typing.Infer
import Tandoori.Typing.Show
    
typecheckMod mod = runDyn $ do
                     env <- getSession
                     (limports, ltydecls, group) <- liftIO $ runScope env mod
                     return $ infer (map unLoc ltydecls) group
                                                                    
main' [src_filename] = do mod <- parseMod src_filename
                          (c, errors) <- typecheckMod mod
                          if not(null errors)
                            then mapM_ (\ error -> printErrs $ ppr error $ mkErrStyle neverQualify) errors
                            else return ()
                          case c of
                            Just (ctxt, m) -> printCtxt ctxt
                            Nothing -> return ()
                          return c

main' _ = error "Usage: tandoori filename.hs" 

main = do args <- getArgs
          main' args
