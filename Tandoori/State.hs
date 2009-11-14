module Tandoori.State (Stateful, StatefulT, mkState, mkTv, addError, getErrors) where

import Tandoori
import Tandoori.Errors
    
import HsTypes
import Name
import Unique
import FastString
import HsExpr
import SrcLoc (SrcSpan, mkSrcSpan, mkSrcLoc)

import qualified HsLit as HL
    
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
    
data GlobalState = G { tvcount :: Int,
                       errors  :: [ErrorMessage Name],
                       loc :: Maybe SrcSpan,
                       expr :: Maybe (HsExpr Name)
                     }
                   -- deriving Show

mkState = G { tvcount = 0,
              errors = [],
              loc = Nothing,
              expr = Nothing
            }

type Stateful a = State GlobalState a
type StatefulT s t = StateT s (State GlobalState) t
    
mkTv :: Stateful (HsType Name)
mkTv = do state@G{ tvcount = tvcount } <- get
          let namestr = 't':(show tvcount)
              uname = mkAlphaTyVarUnique tvcount
              name = mkSysTvName uname (mkFastString namestr)
          put state { tvcount = tvcount + 1}
          return (HsTyVar name)


                 
getLoc :: Stateful (Maybe SrcSpan)
getLoc = do G { loc = loc } <- get
            return loc

setLoc :: Maybe SrcSpan -> Stateful ()
setLoc loc = do state <- get
                put state{loc = loc}
                   
withLoc :: SrcSpan -> Stateful a -> Stateful a
withLoc loc m = do loc' <- getLoc
                   setLoc (Just loc)
                   result <- m
                   setLoc loc'
                   return result                  


getExpr :: Stateful (Maybe (HsExpr Name))
getExpr = do G { expr = expr } <- get
             return expr

setExpr :: Maybe (HsExpr Name) -> Stateful ()
setExpr expr = do state <- get
                  put state{expr = expr}
                      
withExpr :: (HsExpr Name) -> Stateful a -> Stateful a
withExpr expr m = do expr' <- getExpr
                     setExpr (Just expr)
                     result <- m
                     setExpr expr'
                     return result

                            
                          
addError :: ErrorContent -> Stateful ()
addError content = do state@G{ errors = errors } <- get
                      loc <- getLoc
                      expr <- getExpr
                      let msg = ErrorMessage loc expr content
                      put state { errors = msg:errors }

getErrors :: Stateful [ErrorMessage Name]
getErrors = do G { errors = errors } <- get
               return errors

testMonads :: Stateful [ErrorMessage Name]
testMonads = do withLoc span $  withExpr (HsLit (HL.HsChar 'a')) $
                        do addError $ OtherMessage "error message FROM SPACE!"
                getErrors
    where span = mkSrcSpan loc loc'
          loc = mkSrcLoc (mkFastString "foo.hs") 1 1
          loc' = mkSrcLoc (mkFastString "foo.hs") 2 3
