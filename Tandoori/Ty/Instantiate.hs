module Tandoori.Ty.Instantiate (instantiateTy) where

import Tandoori
import Tandoori.Ty
import Tandoori.Ty.State
import Tandoori.Ty.Canonize
import Tandoori.Util
import Tandoori.GHC.Internals
    
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

type TvMap = Map.Map TvName TanType
type Instantiate = StateT TvMap Typing
    
lookupTv :: TvName -> Instantiate (Maybe TanType)
lookupTv tvname = gets $ Map.lookup tvname

ensureTv :: TvName -> Instantiate TanType
ensureTv tv = do lookup <- lookupTv tv
                 case lookup of
                   Just ty' -> return ty'
                   Nothing -> do ty' <- lift mkTv
                                 modify (Map.insert tv ty')
                                 return ty'

instantiateCTyM :: (TvName -> Bool) -> CanonizedType -> Instantiate CanonizedType
instantiateCTyM isPoly cty = do ty' <- instantiateTyM isPoly ty
                                lpreds' <- mapM instantiateLPredM lpreds
                                return $ mkCanonizedType ty' lpreds'
    where ty = ctyTy cty
          lpreds = ctyLPreds cty
                   
          instantiatePredM (HsClassP name ltys) = do ltys' <- mapM (instantiateLTyM isPoly) ltys
                                                     return $ HsClassP name ltys'
          instantiateLPredM lpred = liftM genLoc $ instantiatePredM $ unLoc lpred
                                    
instantiateLTyM :: (TvName -> Bool) -> (Located TanType) -> Instantiate (Located TanType)
instantiateLTyM isPoly lty = liftM noLoc $ instantiateTyM isPoly (unLoc lty)

instantiateTyM :: (TvName -> Bool) -> TanType -> Instantiate TanType
instantiateTyM isPoly ty                    | isTyCon ty  = return ty
                                                                   
instantiateTyM isPoly (HsBangTy bang lty)                 = liftM unLoc $ instantiateLTyM isPoly lty

instantiateTyM isPoly ty@(HsTyVar tv)                     = if isPoly tv
                                                              then ensureTv tv
                                                              else return ty
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
                                                                      
instantiateTy :: (TvName -> Bool) -> CanonizedType -> Typing CanonizedType
instantiateTy isPoly cty = evalStateT (instantiateCTyM isPoly cty) Map.empty


                          
-- -- testInstM = do tv <- mkTv
-- --                tv' <- mkTv
-- --                let ty = canonize $ tyCurryFun [tyCurryFun [tv, tv', tyInt], tyList tv, tyList tv']
-- --                ty' <- instantiateTy (const True) ty
-- --                return (ty, ty')
