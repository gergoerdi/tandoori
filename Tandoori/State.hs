{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tandoori.State (Typing, runTyping, mkTv, addError, withLoc, withSrc, withLSrc) where

import Tandoori
import Tandoori.Errors

import Tandoori.GHC.Internals
    
import Control.Monad.RWS
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Control.Arrow (first, second)
    
newtype Typing a = Typing { rws :: RWS (SrcSpan, Maybe ErrorSource) [ErrorMessage] Int a} deriving Monad

runTyping :: Typing a -> (a, [ErrorMessage])
runTyping typing = let (result, _, output) = (runRWS . rws) typing (noSrcSpan, Nothing) 0
                   in (result, output)                                           

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
                 
getSpan :: Typing SrcSpan
getSpan = Typing $ asks fst
          
withLoc :: SrcSpan -> Typing a -> Typing a
withLoc srcspan = Typing . (local $ first $ const srcspan) . rws

getSrc :: Typing (Maybe ErrorSource)
getSrc = Typing $ asks snd

withSrc :: Outputable e => e -> Typing a -> Typing a
withSrc src = Typing . (local $ second $ const $ Just $ ErrorSource src) . rws

withLSrc :: Outputable e => Located e -> Typing a -> Typing a
withLSrc (L loc src) = withLoc loc . withSrc src

addError :: ErrorContent -> Typing ()
addError err = do span <- getSpan
                  src <- getSrc
                  let msg = ErrorMessage (ErrorLocation span src) err
                  Typing $ tell [msg]
