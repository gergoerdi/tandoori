module Tandoori.Typing.Instantiate (instantiate, instantiatePolyTy, instantiateTyping) where

import Tandoori.Typing
import Tandoori.Typing.Monad
import Tandoori.Typing.MonoEnv    

import qualified Data.Map as Map
import Control.Monad.State

type TvMap = Map.Map Tv Tv
type Instantiate = StateT TvMap Typing
    
lookupTv :: Tv -> Instantiate (Maybe Tv)
lookupTv = gets . Map.lookup

ensureTv :: Tv -> Instantiate Tv
ensureTv α = do lookup <- lookupTv α
                case lookup of
                  Just α' -> return α'
                  Nothing -> do α' <- lift mkTv
                                modify (Map.insert α α')
                                return α'

instantiateM :: Ty -> Instantiate Ty
instantiateM = mapTy ensureTv
-- instantiateM τ@(TyCon _)     = return τ
-- instantiateM τ@(TyVar α)     = TyVar <$> ensureTv α
-- instantiateM τ@(TyFun τ1 τ2) = liftM2 TyFun (instantiateM τ1) (instantiateM τ2)
-- instantiateM τ@(TyApp τ1 τ2) = liftM2 TyApp (instantiateM τ1) (instantiateM τ2)
-- instantiateM τ@(TyTuple _)   = return τ
                                                                      
instantiatePredM :: OverPred -> Instantiate OverPred
instantiatePredM (cls, τ) = do τ' <- instantiateM τ
                               return (cls, τ')

instantiatePolyPredM :: PolyPred -> Instantiate PolyPred
instantiatePolyPredM (cls, α) = do α' <- ensureTv α
                                   return (cls, α')

runInst inst = evalStateT inst Map.empty

instantiate :: Ty -> Typing Ty
instantiate = runInst . instantiateM
                      
instantiatePolyTy :: PolyTy -> Typing PolyTy
instantiatePolyTy = runInst . instantiatePolyTyM

instantiatePolyTyM :: PolyTy -> Instantiate PolyTy
instantiatePolyTyM (PolyTy ctx τ) = liftM2 PolyTy (mapM instantiatePolyPredM ctx) (instantiateM τ)

instantiateTypingM :: (MonoEnv, Ty) -> Instantiate (MonoEnv, Ty)
instantiateTypingM (m, τ) = do τ' <- instantiateM τ
                               m' <- mapMonoM ensureTv m
                               return (m', τ')

instantiateTyping :: (MonoEnv, Ty) -> Typing (MonoEnv, Ty)
instantiateTyping = runInst . instantiateTypingM
