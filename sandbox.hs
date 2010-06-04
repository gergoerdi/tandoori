module Main where

import Tandoori.GHC    
import Tandoori.GHC.Parse
import Tandoori.GHC.Scope
-- import Tandoori.Kludge.Show

import Tandoori.Ty.State
    
import GHC    
import RdrHsSyn
import Outputable
import IOEnv

import System.Environment    
    
import Tandoori.Ty.Infer
import Tandoori.Ty.Ctxt
import Tandoori.Ty
import Tandoori.Ty.MonoEnv
    
import Tandoori.Ty.DataType
import Tandoori.Ty.ClassDecl

import qualified Data.Map as Map
import Control.Monad.Writer (runWriterT)
import Control.Monad (liftM)
    
import IPPrint

--
import Tandoori.Ty.Unify
import Tandoori.GHC.Internals
import Bag
--
    
src_filename = "input/declare.hs"

typecheckMod mod = runDyn $ do
                     env <- getSession
                     (limports, ltydecls, group) <- liftIO $ runScope env mod
                     let tydecls = map unLoc ltydecls
                         cons = concatMap constructorsFromDecl tydecls
                         cg = mkClassGraph tydecls
                         classdecls = map funsFromClassDecl $ sortClassDecls cg
                         classinfo = mkClassInfo cg
                         classfunSigs = concatMap fst classdecls
                         -- c = addUserDecls (mkCtxt cons classinfo) (concatMap fst classdecls)
                             
                     let infer = do
                              runWriterT $ mapM_ inferBinds $ map snd classdecls
                              liftM fst $ inferValBinds (hs_valds group) $ askCtxt
                         withDecls = withCons cons . withClasses classinfo . withUserDecls classfunSigs
                     return $ runTyping $ withDecls infer

main' [src_filename] = do mod <- parseMod src_filename
                          (c, errors) <- typecheckMod mod
                          if not(null errors)
                            then mapM_ (\ error -> printErrs $ ppr error $ mkErrStyle neverQualify) errors
                            else return ()
                          printCtxt c
                          return c

main' _ = error "Usage: tandoori filename.hs" 

main = do args <- getArgs
          main' args

test = main' [src_filename]                
                
-- test = do p <- main' ["input/cikk.hs"]
--           let tyFoo = snd $ snd $ (Map.toList $ polyVars p)!!0
--               ltyId = snd $ (Map.toList $ userdecls p)!!1
--               tyId = unLoc ltyId
--               HsTyVar nGen = tyFoo
--               HsForAllTy _ _ _ (L _ (HsFunTy _ (L _ (HsTyVar nUser)))) = tyId
--           return (tyFoo, tyId)
