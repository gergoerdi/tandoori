module Tandoori.Ty.Instantiate (instantiateTy, instantiateTyM, newInstantiator) where

import Tandoori
import Tandoori.State
import Language.Haskell.Syntax
import qualified Data.Map as Map
import Control.Monad.State
    
type Instantiator = Map.Map TvName HsType

newInstantiator :: Instantiator
newInstantiator = Map.empty    
    
lookupTv :: TvName -> StatefulT Instantiator (Maybe HsType)
lookupTv tvname = do state <- get
                     return $ Map.lookup tvname state

addTv :: TvName -> HsType -> StatefulT Instantiator ()
addTv tvname tv' = do state <- get
                      put (Map.insert tvname tv' state)

ensureTvInst :: TvName -> StatefulT Instantiator HsType
ensureTvInst tvname = do mtv <- lookupTv tvname
                         case mtv of
                           Just tv -> return tv
                           Nothing -> do tv <- lift $ createTv
                                         addTv tvname tv
                                         return tv
                                              
instantiateTyM :: (TvName -> Bool) -> HsType -> StatefulT Instantiator HsType
instantiateTyM isPoly ty@(HsTyVar name)    | isPoly name = ensureTvInst name
                                           | otherwise   = return ty
instantiateTyM isPoly (HsTyFun left right)               = do left' <- instantiateTyM isPoly left
                                                              right' <- instantiateTyM isPoly right
                                                              return (HsTyFun left' right')
instantiateTyM isPoly (HsTyTuple tys)                    = do tys' <- mapM (instantiateTyM isPoly) tys
                                                              return (HsTyTuple tys')
instantiateTyM isPoly (HsTyApp ty param)                 = do ty' <- instantiateTyM isPoly ty
                                                              param' <- instantiateTyM isPoly param
                                                              return (HsTyApp ty' param')
instantiateTyM _      ty@(HsTyCon _)                     = return ty

instantiateTy :: (TvName -> Bool) -> HsType -> Stateful HsType
instantiateTy isPoly ty = evalStateT (instantiateTyM isPoly ty) newInstantiator
