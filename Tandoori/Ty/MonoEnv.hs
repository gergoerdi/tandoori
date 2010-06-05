module Tandoori.Ty.MonoEnv where
--module Tandoori.Ty.MonoEnv(MonoEnv, justType, typedAs, monoVars, combineMonos, removeMonoVars, filterMonoVars, mapMono, mapMonoM) where

import Tandoori
import Tandoori.Ty
import Tandoori.Ty.Canonize
    
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
    
newtype MonoEnv = MonoEnv (Map.Map VarName CanonizedType) -- deriving Show

emptyMono :: MonoEnv
emptyMono = MonoEnv (Map.empty)

justType :: CanonizedType -> (MonoEnv, CanonizedType)
justType ty = (emptyMono, ty)

typedAs :: VarName -> CanonizedType -> (MonoEnv, CanonizedType)
name `typedAs` cty = (addMonoVar emptyMono (name, cty), cty)
            
addMonoVar :: MonoEnv -> (VarName, CanonizedType) -> MonoEnv
addMonoVar (MonoEnv m) (name, cty) = MonoEnv (Map.insert name cty m)

getMonoVar :: MonoEnv -> VarName -> Maybe CanonizedType
getMonoVar (MonoEnv m) name = Map.lookup name m

getMonoVars :: MonoEnv -> [(VarName, CanonizedType)]
getMonoVars (MonoEnv m) = Map.toList m

combineMono :: MonoEnv -> MonoEnv -> MonoEnv
combineMono (MonoEnv m) (MonoEnv m') = MonoEnv $ Map.union m m'

combineMonos :: [MonoEnv] -> MonoEnv
combineMonos ms = foldl combineMono emptyMono ms

removeMonoVars :: MonoEnv -> [VarName] -> MonoEnv
removeMonoVars (MonoEnv m) names = MonoEnv $ foldl removeMonoVar m names
    where removeMonoVar = flip Map.delete

filterMonoVars :: (VarName -> CanonizedType -> Bool) -> MonoEnv -> MonoEnv
filterMonoVars p (MonoEnv m) = MonoEnv $ Map.filterWithKey p m 

mapMono :: (CanonizedType -> CanonizedType) -> MonoEnv -> MonoEnv
mapMono f (MonoEnv m) = MonoEnv $ Map.map f m                               

mapMonoM :: Monad m => (CanonizedType -> m CanonizedType) -> MonoEnv -> m MonoEnv
mapMonoM f (MonoEnv m) = do let kvs = Map.toList m
                            kvs' <- mapM f' kvs
                            return $ MonoEnv $ Map.fromList kvs'
    where f' (k, v) = do v' <- f v
                         return $ (k, v')

