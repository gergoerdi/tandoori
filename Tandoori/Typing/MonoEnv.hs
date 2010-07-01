{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tandoori.Typing.MonoEnv where

import Tandoori
import Tandoori.Typing

import Data.Monoid    
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
    
newtype MonoEnv = MonoEnv{ unMonoEnv :: Map.Map VarName PolyTy } deriving Monoid

justType :: a -> (MonoEnv, a)
justType x = pure x

typedAs :: VarName -> PolyTy -> (MonoEnv, PolyTy)
v `typedAs` σ = (addMonoVar mempty (v, σ), σ)
                
addMonoVar :: MonoEnv -> (VarName, PolyTy) -> MonoEnv
addMonoVar (MonoEnv m) (v, σ) = MonoEnv (Map.insert v σ m)

getMonoVar :: MonoEnv -> VarName -> Maybe PolyTy
getMonoVar (MonoEnv m) name = Map.lookup name m

getMonoVars :: MonoEnv -> [(VarName, PolyTy)]
getMonoVars (MonoEnv m) = Map.toList m

combineMono :: MonoEnv -> MonoEnv -> MonoEnv
combineMono (MonoEnv m) (MonoEnv m') = MonoEnv $ Map.union m m'

combineMonos :: [MonoEnv] -> MonoEnv
combineMonos ms = mconcat ms -- foldl combineMono mempty ms

removeMonoVars :: MonoEnv -> [VarName] -> MonoEnv
removeMonoVars (MonoEnv m) names = MonoEnv $ foldl removeMonoVar m names
    where removeMonoVar = flip Map.delete

filterMonoVars :: (VarName -> PolyTy -> Bool) -> MonoEnv -> MonoEnv
filterMonoVars p (MonoEnv m) = MonoEnv $ Map.filterWithKey p m 

mapMono :: (PolyTy -> PolyTy) -> MonoEnv -> MonoEnv
mapMono f (MonoEnv m) = MonoEnv $ Map.map f m                               

-- TODO: Foldable/Traversable instead                        
mapMonoM :: Monad m => (PolyTy -> m PolyTy) -> MonoEnv -> m MonoEnv
mapMonoM f (MonoEnv m) = do let kvs = Map.toList m
                            kvs' <- mapM f' kvs
                            return $ MonoEnv $ Map.fromList kvs'
    where f' (k, v) = do v' <- f v
                         return $ (k, v')

