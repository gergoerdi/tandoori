module Tandoori.Ty.Instantiate (instantiateTy, instantiateTyM, newInstantiator, testInstM) where

import Tandoori
import Tandoori.State
import Tandoori.Ty
    
import SrcLoc    
import HsTypes
    
import qualified Data.Map as Map
import Control.Monad.State
    
type Instantiator = Map.Map TvName TanType

newInstantiator :: Instantiator
newInstantiator = Map.empty    
    
lookupTv :: TvName -> StatefulT Instantiator (Maybe TanType)
lookupTv tvname = do state <- get
                     return $ Map.lookup tvname state

addTv :: TvName -> TanType -> StatefulT Instantiator ()
addTv tvname tv' = do state <- get
                      put (Map.insert tvname tv' state)

ensureTvInst :: TvName -> StatefulT Instantiator TanType
ensureTvInst tvname = do mtv <- lookupTv tvname
                         case mtv of
                           Just tv -> return tv
                           Nothing -> do tv <- lift $ mkTv
                                         addTv tvname tv
                                         return tv                                                
                                                
instantiateLTyM :: (TvName -> Bool) -> (Located TanType) -> StatefulT Instantiator (Located TanType)
instantiateLTyM isPoly lty = liftM noLoc $ instantiateTyM isPoly (unLoc lty)
                                                
instantiateTyM :: (TvName -> Bool) -> TanType -> StatefulT Instantiator TanType
instantiateTyM _      ty                    | isTyCon ty  = return ty
instantiateTyM isPoly (HsForAllTy _ _ _ lty)              = liftM unLoc $ instantiateLTyM isPoly lty
instantiateTyM isPoly (HsBangTy bang lty)                 = liftM unLoc $ instantiateLTyM isPoly lty

instantiateTyM isPoly ty@(HsTyVar name)     | isPoly name = ensureTvInst name
                                            | otherwise   = return ty
                                                                 
instantiateTyM isPoly (HsAppTy lx ly)                     = do lx' <- instantiateLTyM isPoly lx
                                                               ly' <- instantiateLTyM isPoly ly
                                                               return $ HsAppTy lx' ly'
instantiateTyM isPoly (HsFunTy lx ly)                     = do lx' <- instantiateLTyM isPoly lx
                                                               ly' <- instantiateLTyM isPoly ly
                                                               return $ HsFunTy lx' ly'
instantiateTyM isPoly (HsListTy lty)                      = do lty' <- instantiateLTyM isPoly lty
                                                               return $ HsListTy lty'
instantiateTyM isPoly (HsTupleTy box ltys)                = do ltys' <- mapM (instantiateLTyM isPoly) ltys
                                                               return (HsTupleTy box ltys')

instantiateTy :: (TvName -> Bool) -> TanType -> Stateful TanType
instantiateTy isPoly ty = evalStateT (instantiateTyM isPoly ty) newInstantiator



                          
testInstM = do tv <- mkTv
               tv' <- mkTv
               let ty = tyCurryFun [tyCurryFun [tv, tv', tyInt], tyList tv, tyList tv']
               ty' <- instantiateTy (const True) ty
               return (ty, ty')
