{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tandoori.Ty.State (Typing, runTyping,
                          mkTv,
                          askCtxt,
                          askCon, askUserDecl, askClassInfo, askPolyVar, askForcedMonoVars,
                          withUserDecls, withMonoVars, withPolyVars,
                          withCons, withClasses,
                          addError, withLoc, withSrc, withLSrc) where

import Tandoori
import Tandoori.Errors
import Tandoori.GHC.Internals
import Tandoori.Ty
import Tandoori.Ty.ClassDecl
import Tandoori.Ty.Ctxt
import Tandoori.Ty.MonoEnv
import Control.Monad.RWS
import Data.Maybe
import qualified Data.Set as Set

data R = R { loc :: SrcSpan,
             src :: Maybe ErrorSource,
             ctxt :: Ctxt }

newtype Typing a = Typing { rws :: RWS R [ErrorMessage] Int a} deriving Monad

runTyping :: Typing a -> (a, [ErrorMessage])
runTyping typing = let (result, _, output) = (runRWS . rws) typing r 0
                   in (result, output)
    where r = R { loc = noSrcSpan,
                  src = Nothing,
                  ctxt = mkCtxt }

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
    where setLoc r = r{loc = srcspan}

withSrc :: Outputable e => e -> Typing a -> Typing a
withSrc src = Typing . (local setSrc) . rws
    where setSrc r = r{src = Just $ ErrorSource src}

withLSrc :: Outputable e => Located e -> Typing a -> Typing a
withLSrc (L loc src) = withLoc loc . withSrc src

addError :: ErrorContent -> Typing ()
addError err = do loc <- Typing $ asks loc
                  src <- Typing $ asks src
                  let msg = ErrorMessage (ErrorLocation loc src) err
                  Typing $ tell [msg]

liftCtxt select = Typing . asks $ select . ctxt                         
                         
askCtxt = Typing $ asks ctxt
                 
askCon = ap (liftCtxt getCon) . return         
askUserDecl = ap (liftCtxt getUserDecl) . return
askPolyVar = ap (liftCtxt getPolyVar) . return
askForcedMonoVars = liftCtxt forcedMonoVars
askClassInfo = liftCtxt classinfo
               
withUserDecls :: [LSig Name] -> Typing a -> Typing a
withUserDecls sigs = Typing . local add . rws
    where add r@R{ctxt = c} = r{ctxt = addUserDecls c sigs}

withMonoVars :: Set.Set VarName -> Typing a -> Typing a
withMonoVars ns = Typing . local add . rws
    where add r@R{ctxt = c} = r{ctxt = forceMonoVars c ns}

withPolyVars :: [(VarName, (MonoEnv, CanonizedType))] -> Typing a -> Typing a
withPolyVars vars = Typing . local add . rws
    where add r@R{ctxt = c} = r{ctxt = addPolyVars c vars}

withCons cons = Typing . local add . rws
    where add r@R{ctxt = c} = r{ctxt = setCons cons c}
                              
withClasses classinfo = Typing . local add . rws
    where add r@R{ctxt = c} = r{ctxt = setClasses classinfo c}
