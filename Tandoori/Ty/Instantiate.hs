module Tandoori.Ty.Instantiate (instantiateTy, instantiateTyM, newInstantiator, testInstM) where

import Tandoori
import Tandoori.State
import Tandoori.Ty
import Tandoori.Util
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
                                                
instantiateLTyM :: (Located TanType) -> StatefulT Instantiator (Located TanType)
instantiateLTyM lty = liftM noLoc $ instantiateTyM (unLoc lty)
                                                
instantiateTyM :: TanType -> StatefulT Instantiator TanType
instantiateTyM ty                          | isTyCon ty  = return ty
instantiateTyM (HsForAllTy e _ lctxt lty)                = do let lpreds = unLoc lctxt
                                                              lty' <- instantiateLTyM lty
                                                              lpreds' <- mapM instantiateLPredM lpreds
                                                              let lctxt' = genLoc lpreds'
                                                              return $ HsForAllTy e noBinder lctxt' lty'
    where instantiatePredM (HsClassP name ltys) = do ltys' <- mapM instantiateLTyM ltys
                                                     return $ HsClassP name ltys'
          instantiateLPredM lpred = liftM genLoc $ instantiatePredM $ unLoc lpred

                                                                   
instantiateTyM (HsBangTy bang lty)                       = liftM unLoc $ instantiateLTyM lty

instantiateTyM ty@(HsTyVar name)                         = ensureTvInst name
                                                                 
instantiateTyM (HsAppTy lx ly)                           = do lx' <- instantiateLTyM lx
                                                              ly' <- instantiateLTyM ly
                                                              return $ HsAppTy lx' ly'
instantiateTyM (HsFunTy lx ly)                           = do lx' <- instantiateLTyM lx
                                                              ly' <- instantiateLTyM ly
                                                              return $ HsFunTy lx' ly'
instantiateTyM (HsListTy lty)                            = do lty' <- instantiateLTyM lty
                                                              return $ HsListTy lty'
instantiateTyM (HsTupleTy box ltys)                      = do ltys' <- mapM instantiateLTyM ltys
                                                              return $ HsTupleTy box ltys'
instantiateTyM (HsParTy lty)                             = do lty' <- instantiateLTyM lty
                                                              return $ HsParTy lty'
                                                                      
instantiateTy :: TanType -> Stateful TanType
instantiateTy ty = evalStateT (instantiateTyM ty) newInstantiator



                          
testInstM = do tv <- mkTv
               tv' <- mkTv
               let ty = tyCurryFun [tyCurryFun [tv, tv', tyInt], tyList tv, tyList tv']
               ty' <- instantiateTy ty
               return (ty, ty')
