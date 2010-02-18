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
import Tandoori.Ty.Ctxt
import Tandoori.Ty
import Tandoori.Ty.DataType

import qualified Data.Map as Map
    
import IPPrint

--
import Tandoori.Ty.Unify
import Tandoori.GHC.Internals
import Bag
--
    
src_filename = "input/declare.hs"

typecheckMod mod = runDyn $ do
                     env <- getSession
                     (tydecls, group) <- liftIO $ runScope env mod
                     let cons = concat $ map constructorsFromDecl $ map unLoc tydecls
                         c = mkCtxt cons
                     let infer = do
                              (ns, c', _) <- inferValBinds c $ hs_valds group
                              errors <- getErrors
                              return $ (c', errors)
                     return $ evalState infer mkState

foobar = do mod <- parseMod "input/class-cascade-simple.hs"
            runDyn $ do
              env <- getSession
              (tydecls, group) <- liftIO $ runScope env mod
              let cons = concat $ map constructorsFromDecl $ map unLoc tydecls
                  c = mkCtxt cons
              let ValBindsOut [(_, bag1), (_, bag2)] sigs = hs_valds group
                  [L _ FunBind { fun_id = L _ funName }] = bagToList bag1
                  [L _ bind2] = bagToList bag2
              let infer = do
                       alpha <- mkTv
                       let ty = tyCurryFun [alpha, alpha]
                           lctxt = noLoc [noLoc $ HsClassP numClassName [noLoc alpha]]
                           ty' = HsForAllTy undefined undefined lctxt (noLoc ty)
                           sig = TypeSig (noLoc funName) (noLoc ty')
                           c' = addUserDecls c [noLoc sig]                                
                       return $ c'
              return $ (evalState infer mkState, bind2)

testFromContext c b = evalState infer mkState
    where infer = do (names, m) <- inferBind c b
                     return m
              
foobar' =  do (c, b) <- foobar
              return $ (c, testFromContext c b)
              
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

test = do p <- main' ["input/cikk.hs"]
          let tyFoo = snd $ snd $ (Map.toList $ polyVars p)!!0
              ltyId = snd $ (Map.toList $ userdecls p)!!1
              tyId = unLoc ltyId
              HsTyVar nGen = tyFoo
              HsForAllTy _ _ _ (L _ (HsFunTy _ (L _ (HsTyVar nUser)))) = tyId
          return (tyFoo, tyId)
