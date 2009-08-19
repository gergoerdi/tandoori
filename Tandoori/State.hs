module Tandoori.State (ErrorMessage(..), ErrorContent(..), Stateful, newState, withLoc, withExpr, addError, getErrors, createTv) where

import Tandoori    
import Control.Monad.State
import Language.Haskell.Syntax
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
    
data GlobalState = G { tvcount :: Integer,
                       errors :: [ErrorMessage],
                       loc :: Maybe SrcLoc,
                       expr :: Maybe HsExp
                     }
                   deriving Show

newState = G { tvcount = 0, errors = [], loc = Nothing,  expr = Nothing }

data ErrorMessage = ErrorMessage (Maybe SrcLoc) (Maybe HsExp) ErrorContent
                    deriving Show
                             
data ErrorContent = OtherMessage String
                  | UndefinedConstructor ConName
                    deriving Show
    
type Stateful a = State GlobalState a

createTv :: Stateful HsType
createTv = do state@G{ tvcount = tvcount } <- get
              put state { tvcount = tvcount + 1}
              return (HsTyVar (HsSymbol ("t" ++ (show tvcount))))    


                     
getLoc :: Stateful (Maybe SrcLoc)
getLoc = do G { loc = loc } <- get
            return loc

setLoc :: Maybe SrcLoc -> Stateful ()
setLoc loc = do state <- get
                put state{loc = loc}
                   
withLoc :: SrcLoc -> Stateful a -> Stateful a
withLoc loc m = do loc' <- getLoc
                   setLoc (Just loc)
                   result <- m
                   setLoc loc'
                   return result                  


getExpr :: Stateful (Maybe HsExp)
getExpr = do G { expr = expr } <- get
             return expr

setExpr :: Maybe HsExp -> Stateful ()
setExpr expr = do state <- get
                  put state{expr = expr}
                      
withExpr :: HsExp -> Stateful a -> Stateful a
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

getErrors :: Stateful [ErrorMessage]
getErrors = do G { errors = errors } <- get
               return errors

testMonads = do withLoc (SrcLoc {srcFilename = "foo.hs", srcLine = 1, srcColumn = 1}) $
                        withExpr (HsLit (HsChar 'a')) $
                                 do addError $ OtherMessage "error message FROM SPACE!"
                getErrors
