module Tandoori.Ty.MonoEnv where
--module Tandoori.Ty.MonoEnv(MonoEnv, justType, typedAs, monoVars, combineMonos, removeMonoVars, filterMonoVars, mapMono, mapMonoM) where

import Tandoori
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
    
newtype MonoEnv = MonoEnv (Map.Map VarName TanType) -- deriving Show

emptyMono :: MonoEnv
emptyMono = MonoEnv (Map.empty)

justType :: TanType -> (MonoEnv, TanType)
justType ty = (emptyMono, ty)

typedAs :: VarName -> TanType -> (MonoEnv, TanType)
name `typedAs` ty = (addMonoVar emptyMono (name, ty), ty)
            
addMonoVar :: MonoEnv -> (VarName, TanType) -> MonoEnv
addMonoVar (MonoEnv m) (name, ty) = MonoEnv (Map.insert name ty m)

getMonoVar :: MonoEnv -> VarName -> Maybe TanType
getMonoVar (MonoEnv m) name = Map.lookup name m

monoVars :: MonoEnv -> [(VarName, TanType)]
monoVars (MonoEnv m) = Map.toList m

combineMono :: MonoEnv -> MonoEnv -> MonoEnv
combineMono (MonoEnv m) (MonoEnv m') = MonoEnv $ Map.union m m'

combineMonos :: [MonoEnv] -> MonoEnv
combineMonos ms = foldl combineMono emptyMono ms

removeMonoVars :: MonoEnv -> [VarName] -> MonoEnv
removeMonoVars (MonoEnv m) names = MonoEnv $ foldl removeMonoVar m names
    where removeMonoVar = flip Map.delete

filterMonoVars :: (VarName -> TanType -> Bool) -> MonoEnv -> MonoEnv
filterMonoVars p (MonoEnv m) = MonoEnv $ Map.filterWithKey p m 

mapMono :: (TanType -> TanType) -> MonoEnv -> MonoEnv
mapMono f (MonoEnv m) = MonoEnv $ Map.map f m                               

mapMonoM :: Monad m => (TanType -> m TanType) -> MonoEnv -> m MonoEnv
mapMonoM f (MonoEnv m) = do let kvs = Map.toList m
                            kvs' <- mapM f' kvs
                            return $ MonoEnv $ Map.fromList kvs'
    where f' (k, v) = do v' <- f v
                         return $ (k, v')

