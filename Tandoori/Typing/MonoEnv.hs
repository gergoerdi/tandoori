module Tandoori.Typing.MonoEnv (MonoEnv, getMonoTy, setMonoTy, mapMonoM, getMonoVars, getMonoVar, justType, typedAs, addMonoVar, filterMonoVars, combineMonos) where

import Tandoori
import Tandoori.Typing

import Data.Monoid    
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
    
data MonoEnv = MonoEnv{ 
    ty :: Maybe PolyTy,
    monovars :: Map VarName PolyTy }

getMonoTy = ty
setMonoTy m σ = m{ty = Just σ}

empty :: PolyTy -> MonoEnv
empty σ = MonoEnv{ ty = Just σ,
                   monovars = mempty }

justType :: PolyTy -> (MonoEnv, PolyTy)
justType σ = (empty σ, σ)

typedAs :: VarName -> PolyTy -> (MonoEnv, PolyTy)
v `typedAs` σ = (addMonoVar (empty σ) (v, σ), σ)
                
addMonoVar :: MonoEnv -> (VarName, PolyTy) -> MonoEnv
addMonoVar m (v, σ) = m{monovars = Map.insert v σ (monovars m)}

getMonoVar :: MonoEnv -> VarName -> Maybe PolyTy
getMonoVar m name = Map.lookup name (monovars m)

getMonoVars :: MonoEnv -> [(VarName, PolyTy)]
getMonoVars m = Map.toList (monovars m)

combineMonos :: [MonoEnv] -> MonoEnv
combineMonos ms = MonoEnv{ty = Nothing,
                          monovars = mconcat $ map monovars ms}

removeMonoVars :: MonoEnv -> [VarName] -> MonoEnv
removeMonoVars m names = m{monovars = foldl removeMonoVar (monovars m) names}
    where removeMonoVar = flip Map.delete

filterMonoVars :: (VarName -> PolyTy -> Bool) -> MonoEnv -> MonoEnv
filterMonoVars p m = m{monovars = Map.filterWithKey p (monovars m)}

mapMono :: (PolyTy -> PolyTy) -> MonoEnv -> MonoEnv
mapMono f m = m{ty = fmap f (ty m),
                monovars = fmap f (monovars m)}

-- TODO: Foldable/Traversable instead                        
mapMonoM :: Monad m => (PolyTy -> m PolyTy) -> MonoEnv -> m MonoEnv
mapMonoM f m = do let kvs = Map.toList (monovars m)
                  kvs' <- mapM f' kvs
                  ty' <- f'' (ty m)
                  return m{ty = ty', 
                           monovars = Map.fromList kvs'}
    where f' (k, v) = do v' <- f v
                         return $ (k, v')
                         
          f'' Nothing = return Nothing
          f'' (Just σ) = do σ' <- f σ
                            return $ Just σ'

