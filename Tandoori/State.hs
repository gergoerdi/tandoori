module Tandoori.State (Stateful, StatefulT, mkState, mkTv, addError, getErrors, withLoc) where

import Tandoori
import Tandoori.Errors
    
import HsTypes
import Name
import Unique
import FastString
import HsExpr
import SrcLoc (SrcSpan, mkSrcSpan, mkSrcLoc)
import Outputable
import qualified SrcLoc as L
    
import qualified HsLit as HL
    
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
    
data GlobalState = G { tvcount :: Int,
                       errors  :: [ErrorMessage],
                       loc :: SrcSpan,
                       src :: Maybe ErrorSource
                     }
                   -- deriving Show

mkState = G { tvcount = 0,
              errors = [],
              loc = L.noSrcSpan,
              src = Nothing
            }

type Stateful a = State GlobalState a
type StatefulT s t = StateT s (State GlobalState) t
    
mkTv :: Stateful TanType
mkTv = do state@G{tvcount = tvcount} <- get
          let namestr = 't':(show tvcount)
              uname = mkAlphaTyVarUnique tvcount
              name = mkSysTvName uname (mkFastString namestr)
          put state{ tvcount = tvcount + 1}
          return (HsTyVar name)


                 
getLoc :: Stateful SrcSpan
getLoc = do G { loc = loc } <- get
            return loc

setLoc :: SrcSpan -> Stateful ()
setLoc loc' = do state <- get
                 put state{loc = loc'}
                   
withLoc :: SrcSpan -> Stateful a -> Stateful a
withLoc loc' m = do loc <- getLoc
                    setLoc loc'
                    result <- m
                    setLoc loc
                    return result                  

getSrc :: Stateful (Maybe ErrorSource)
getSrc = do G { src = src } <- get
            return src

setSrc :: Outputable e => Maybe e -> Stateful ()
setSrc Nothing = do state <- get
                    put state{src = Nothing}
setSrc (Just src') = do state <- get
                        put state{src = Just $ ErrorSource src'}
          
-- setExpr :: Maybe (HsExpr Name) -> Stateful ()
-- setExpr expr = do state <- get
--                   put state{expr = expr}
                      
-- withExpr :: (HsExpr Name) -> Stateful a -> Stateful a
-- withExpr expr m = do expr' <- getExpr
--                      setExpr (Just expr)
--                      result <- m
--                      setExpr expr'
--                      return result

                            
                          
addError :: ErrorContent -> Stateful ()
addError content = do state@G{errors = errors} <- get
                      loc <- getLoc
                      src <- getSrc
                      let msg = ErrorMessage (ErrorLocation loc src) content
                      put state{ errors = msg:errors}

getErrors :: Stateful [ErrorMessage]
getErrors = do G { errors = errors } <- get
               return errors
