module Tandoori.Ty.Printer (niceTy, niceTyM, newTyPrinter) where

import Tandoori.State
    
import Language.Haskell.Syntax
import Control.Monad.State
import qualified Data.Map as Map
import Data.Char

data TyPrinter = TyPrinter { tvmap :: Map.Map HsName HsName }
                 deriving (Show)

newTyPrinter :: TyPrinter
newTyPrinter = TyPrinter { tvmap = Map.empty }

readableTvName :: HsName -> State TyPrinter HsName
readableTvName name = do state@(TyPrinter{tvmap=tvmap}) <- get                                   
                         case Map.lookup name tvmap of
                           Just name' -> return name'
                           Nothing  -> do let name' = s (Map.size tvmap)
                                          put state {tvmap = (Map.insert name name' tvmap)}
                                          return name'
                               where s n | n < 26 = HsIdent [chr ((ord 'a') + n)]
                                         | otherwise = HsIdent ("T" ++ (show n))
                                  
niceTyM :: HsType -> State TyPrinter HsType
niceTyM (HsTyVar name)       = do name' <- readableTvName name
                                  return (HsTyVar name')
niceTyM (HsTyFun left right) = do left' <- niceTyM left
                                  right' <- niceTyM right
                                  return (HsTyFun left' right')
niceTyM (HsTyTuple tys)      = do tys' <- mapM niceTyM tys
                                  return (HsTyTuple tys')
niceTyM (HsTyApp ty param)   = do ty' <- niceTyM ty
                                  param' <- niceTyM param
                                  return (HsTyApp ty' param')
niceTyM tycon@(HsTyCon _)    = return tycon


niceTy :: HsType -> HsType
niceTy ty = evalState (niceTyM ty) newTyPrinter
