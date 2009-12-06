module Tandoori.State (Stateful, StatefulT, mkState, mkTv, addError, getErrors, withLoc, withSrc) where

import Tandoori
import Tandoori.Errors

import Tandoori.GHC.Internals
    
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
    
data GlobalState = G { tvcount :: Int,
                       errors  :: [ErrorMessage],
                       srcspan :: SrcSpan,
                       src :: Maybe ErrorSource
                     }
                   -- deriving Show

mkState = G { tvcount = 0,
              errors = [],
              srcspan = noSrcSpan,
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


                 
getSpan :: Stateful SrcSpan
getSpan = do G { srcspan = srcspan } <- get
             return srcspan

setSpan :: SrcSpan -> Stateful ()
setSpan srcspan = do state <- get
                     put state{srcspan = srcspan}
                   
withLoc :: SrcSpan -> Stateful a -> Stateful a
withLoc srcspan' m = do srcspan <- getSpan
                        setSpan srcspan'
                        result <- m
                        setSpan srcspan
                        return result                  

getSrc :: Stateful (Maybe ErrorSource)
getSrc = do G { src = src } <- get
            return src

setSrc :: Outputable e => Maybe e -> Stateful ()
setSrc Nothing = do state <- get
                    put state{src = Nothing}
setSrc (Just src') = do state <- get
                        put state{src = Just $ ErrorSource src'}

restoreSrc :: Maybe ErrorSource -> Stateful ()
restoreSrc Nothing = do state <- get
                        put state{src = Nothing}
restoreSrc (Just src) = do state <- get
                           put state{src = Just src}
                            
withSrc :: Outputable e => e -> Stateful a -> Stateful a
withSrc src m = do src' <- getSrc
                   setSrc (Just src)
                   result <- m
                   restoreSrc src'
                   return result
                            
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
                      span <- getSpan
                      src <- getSrc
                      let msg = ErrorMessage (ErrorLocation span src) content
                      put state{ errors = msg:errors}

getErrors :: Stateful [ErrorMessage]
getErrors = do G { errors = errors } <- get
               return errors
