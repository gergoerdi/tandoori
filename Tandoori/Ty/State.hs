{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns #-}
module Tandoori.Ty.State (Typing, runTyping,
                          mkTv,
                          askCtxt,
                          askCon, askUserDecl, askBaseClassesOf, askPolyVar, askForcedMonoVars,
                          withUserDecls, withMonoVars, withPolyVars,
                          addError, withLoc, withSrc, withLSrc) where

import Tandoori
import Tandoori.Errors
import Tandoori.GHC.Internals
import Tandoori.Ty
import Tandoori.Ty.Canonize
import Tandoori.Ty.ClassDecl
import Tandoori.Ty.Ctxt
import Tandoori.Ty.MonoEnv
import Control.Monad.RWS
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

import Tandoori.Ty.DataType
import Tandoori.Ty.ClassDecl

data R = R { cons :: Map.Map ConName CanonizedType,
             baseClasses :: BaseClasses,
             ctxt :: Ctxt }

newtype Typing a = Typing { rws :: RWS R [ErrorMessage] Int a} deriving Monad

runTyping :: [LTyClDecl Name] -> Typing a -> (a, [ErrorMessage])
runTyping ltydecls typing = let (result, s', output) = (runRWS . rws) typing r s
                            in (result, output)
    where tydecls = map unLoc ltydecls
          cons = Map.fromList $ concatMap constructorsFromDecl tydecls

          (methods, baseClasses) = getClassInfo ltydecls                 
          methodSigs = concatMap methodDecls methods

          r = R { cons = cons,
                  baseClasses = baseClasses,
                  ctxt = addUserDecls mkCtxt methodSigs }
          s = 0

fresh :: Typing Int
fresh = Typing $ do u <- get
                    modify succ
                    return u
                      
mkTv :: Typing TanType
mkTv = do u <- fresh
          let namestr = 't':(show u)
              uname = mkAlphaTyVarUnique u
              name = mkSysTvName uname (mkFastString namestr)
          return $ HsTyVar name

withLoc :: SrcSpan -> Typing a -> Typing a
withLoc srcspan = Typing . (local setLoc) . rws
    where setLoc r@R{ctxt} = r{ ctxt = ctxt{loc = srcspan } }

withSrc :: Outputable e => e -> Typing a -> Typing a
withSrc src = Typing . (local setSrc) . rws
    where setSrc r@R{ctxt} = r{ ctxt = ctxt{ src = Just $ ErrorSource src } }

withLSrc :: Outputable e => Located e -> Typing a -> Typing a
withLSrc (L loc src) = withLoc loc . withSrc src

addError :: ErrorContent -> Typing ()
addError err = do loc <- Typing $ asks $ loc . ctxt
                  src <- Typing $ asks $ src . ctxt
                  let msg = ErrorMessage (ErrorLocation loc src) err
                  Typing $ tell [msg]

askCtxt = Typing $ asks ctxt                 
askForcedMonoVars = Typing $ asks $ monoVars . ctxt
askCon conname = liftM (Map.lookup conname) $ Typing $ asks cons
askBaseClassesOf cls = do f <- Typing $ asks baseClasses
                          return $ f cls

askUserDecl :: VarName -> Typing (Maybe (Located CanonizedType))                                 
askUserDecl varname = do c <- Typing $ asks ctxt
                         return $ getUserDecl c varname

askPolyVar :: VarName -> Typing (Maybe (MonoEnv, CanonizedType))
askPolyVar varname = do c <- Typing $ asks $ ctxt
                        return $ getPolyVar c varname
               
withUserDecls :: [LSig Name] -> Typing a -> Typing a
withUserDecls sigs = Typing . local add . rws
    where add r@R{ctxt} = r{ ctxt = addUserDecls ctxt sigs }                               

withMonoVars :: Set.Set VarName -> Typing a -> Typing a
withMonoVars vars = Typing . local add . rws
    where add r@R{ctxt} = r{ ctxt = addMonoVars ctxt vars }

withPolyVars :: [(VarName, (MonoEnv, CanonizedType))] -> Typing a -> Typing a
withPolyVars vars = Typing . local add . rws
    where add r@R{ctxt} = r{ ctxt = addPolyVars ctxt vars }
                              
