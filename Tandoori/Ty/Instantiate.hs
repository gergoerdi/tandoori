module Tandoori.Ty.Instantiate (instantiateTy, instantiateTyM, newInstantiator, testInstM) where

import Tandoori
import Tandoori.State
import Tandoori.Ty
import Tandoori.GHC.Internals
    
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
instantiateTyM isPoly (HsForAllTy e lbndrs lctxt lty)     = do let bndrs = map unLoc lbndrs
                                                                   lpreds = unLoc lctxt
                                                               bndrs' <- mapM instantiateTyVarBndrM bndrs
                                                               lpreds' <- mapM instantiateLPredM lpreds
                                                               lty' <- instantiateLTyM isPoly lty
                                                               let lbndrs' = map genLoc bndrs'
                                                                   lctxt' = genLoc lpreds'
                                                               return $ HsForAllTy e lbndrs' lctxt' lty'
    where instantiateTyVarBndrM bind@(UserTyVar name) | isPoly name = do HsTyVar name' <- ensureTvInst name
                                                                         return $ UserTyVar name'
                                                      | otherwise = return bind
                                                                    
          instantiatePredM (HsClassP name ltys) = do ltys' <- mapM (instantiateLTyM isPoly) ltys
                                                     return $ HsClassP name ltys'
          instantiateLPredM lpred = liftM genLoc $ instantiatePredM $ unLoc lpred

                                                                   
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
                                                               return $ HsTupleTy box ltys'
instantiateTyM isPoly (HsParTy lty)                       = do lty' <- instantiateLTyM isPoly lty
                                                               return $ HsParTy lty'
                                                                      
instantiateTy :: (TvName -> Bool) -> TanType -> Stateful TanType
instantiateTy isPoly ty = evalStateT (instantiateTyM isPoly ty) newInstantiator



                          
testInstM = do tv <- mkTv
               tv' <- mkTv
               let ty = tyCurryFun [tyCurryFun [tv, tv', tyInt], tyList tv, tyList tv']
               ty' <- instantiateTy (const True) ty
               return (ty, ty')
