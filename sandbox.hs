module Main where

import Tandoori.GHC    
import Tandoori.GHC.Parse
import Tandoori.GHC.Scope
import Tandoori.Kludge.Show

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
import Tandoori.Ty.InstanceDecl
    
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
                     let (methods, _) = getClassInfo ltydecls
                         linstdecls = hs_instds group
                         instmap = mkInstanceMap linstdecls
                         (L _ (InstDecl (L _ (HsPredTy (HsClassP cls _))) _ _ _)) = linstdecls!!2
                         --pred = HsClassP cls [noLoc $ HsListTy $ noLoc $ ctyTy tyBool]
                         pred = HsClassP cls [noLoc $ HsListTy $ noLoc $ HsListTy $ noLoc $ ctyTy tyBool]
                         pred' = HsClassP cls [noLoc $ ctyTy tyInt]

                         printTest pred@(HsClassP cls [(L _ ty)]) = liftIO $ putStrLn $ unwords ["baseInstancesOf", show cls, show ty,
                                                                                                 "=",
                                                                                                 show $ baseInstancesOf instmap pred]
                                 
                     -- liftIO $ print cls
                     -- liftIO $ mapM_ print linstdecls
                     -- mapM_ printTest [pred, pred']
                     let infer = do
                              runWriterT $ mapM_ inferBinds $ map methodImpls methods
                              liftM fst $ inferValBinds (hs_valds group)
                     return $ runTyping ltydecls linstdecls $ infer

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
