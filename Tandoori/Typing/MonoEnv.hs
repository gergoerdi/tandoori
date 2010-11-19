module Tandoori.Typing.MonoEnv (MonoEnv, noVars, setMonoSrc, getMonoSrc, getMonoTy, setMonoTy, mapMonoM, getMonoVars, getMonoVar, addMonoVar, filterMonoVars, combineMonos) where

import Prelude hiding (mapM)
import Tandoori
import Tandoori.GHC.Internals (SDoc)
import Tandoori.Typing

import Data.Monoid    
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (mapM)
import Control.Monad (liftM)

data MonoEnv = MonoEnv{ 
  source :: Maybe SDoc,
  ty :: Maybe Ty,
  monovars :: Map VarName Ty, 
  preds :: Set OverPred
  }

setMonoSrc m src = m{ source = Just src }
getMonoSrc = source

getMonoTy = ty
setMonoTy m τ = m{ty = Just τ}

empty :: PolyTy -> MonoEnv
empty σ@(PolyTy ctx τ) = MonoEnv{ source = Nothing,
                                  ty = Just τ,
                                  monovars = mempty, 
                                  preds = Set.fromList $ map (fmap TyVar) ctx}

noVars :: MonoEnv
noVars = MonoEnv{ source = Nothing, ty = Nothing, monovars = mempty, preds = mempty}

justType :: PolyTy -> (MonoEnv, Ty)
justType σ@(PolyTy ctx τ) = (empty σ, τ)

typedAs :: VarName -> PolyTy -> (MonoEnv, Ty)
x `typedAs` σ@(PolyTy ctx τ) = (addMonoVar (empty σ) (x, σ), τ)
                
addMonoVar :: MonoEnv -> (VarName, PolyTy) -> MonoEnv
addMonoVar m (x, σ@(PolyTy ctx τ)) = m{monovars = Map.insert x τ (monovars m), preds = Set.fromList (map (fmap TyVar) ctx) `Set.union` (preds m) }

getMonoVar :: MonoEnv -> VarName -> Maybe Ty
getMonoVar m name = Map.lookup name (monovars m)

getMonoVars :: MonoEnv -> [(VarName, Ty)]
getMonoVars m = Map.toList (monovars m)

combineMonos :: [MonoEnv] -> MonoEnv
combineMonos ms = MonoEnv{source = Nothing,
                          ty = Nothing,
                          monovars = mconcat $ map monovars ms,
                          preds = mconcat $ map preds ms}

filterMonoVars :: (VarName -> Ty -> Bool) -> MonoEnv -> MonoEnv
filterMonoVars p m = m{monovars = Map.filterWithKey p (monovars m)}

mapMonoM :: Monad m => (Ty -> m Ty) -> MonoEnv -> m MonoEnv
mapMonoM f m = do monovars' <- mapM f (monovars m)
                  τ' <- case ty m of
                    Nothing -> return Nothing
                    Just τ -> liftM Just $ f τ
                  return m{ty = τ', monovars = monovars'}
