module Tandoori.Typing.Instantiate (instantiate, instantiatePolyTy) where

import Tandoori
import Tandoori.Typing
import Tandoori.Typing.Monad
import Tandoori.Util
import Tandoori.GHC.Internals
    
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad.RWS

type TvMap = Map.Map Tv Tv
type Instantiate = RWST (Tv -> Bool) () TvMap Typing
    
lookupTv :: Tv -> Instantiate (Maybe Tv)
lookupTv = gets . Map.lookup

isPoly :: Tv -> Instantiate Bool
isPoly τ = do f <- ask
              return $ f τ

ensureTv :: Tv -> Instantiate Tv
ensureTv α = do lookup <- lookupTv α
                case lookup of
                  Just α' -> return α'
                  Nothing -> do α' <- lift mkTv
                                modify (Map.insert α α')
                                return α'

instantiateTvM :: Tv -> Instantiate Tv
instantiateTvM α = do poly <- isPoly α
                      if poly then ensureTv α else return α
                                       
instantiateM :: Ty -> Instantiate Ty
instantiateM τ@(TyCon _)     = return τ
instantiateM τ@(TyVar α)     = TyVar <$> instantiateTvM α
instantiateM τ@(TyFun τ1 τ2) = liftM2 TyFun (instantiateM τ1) (instantiateM τ2)
instantiateM τ@(TyApp τ1 τ2) = liftM2 TyApp (instantiateM τ1) (instantiateM τ2)
instantiateM τ@(TyTuple _)   = return τ
                                                                      
instantiatePredM :: OverPred -> Instantiate OverPred
instantiatePredM (cls, τ) = do τ' <- instantiateM τ
                               return (cls, τ')

instantiatePolyPredM :: PolyPred -> Instantiate PolyPred
instantiatePolyPredM (cls, α) = do α' <- instantiateTvM α
                                   return (cls, α')

runInst isPoly inst = fst <$> evalRWST inst isPoly Map.empty

instantiate :: (Tv -> Bool) -> Ty -> Typing Ty
instantiate isPoly = runInst isPoly . instantiateM
                      
instantiateOverTy :: (Tv -> Bool) -> OverTy -> Typing OverTy
instantiateOverTy isPoly (OverTy ctx τ) = runInst isPoly inst
    where inst = liftM2 OverTy (mapM instantiatePredM ctx) (instantiateM τ)
                 
instantiatePolyTy :: (Tv -> Bool) -> PolyTy -> Typing PolyTy
instantiatePolyTy isPoly (PolyTy ctx τ) = runInst isPoly inst
    where inst = liftM2 PolyTy (mapM instantiatePolyPredM ctx) (instantiateM τ)
