module Tandoori.Ty.MonoEnv(MonoEnv, justType, typedAs, (|+|), (|->|), monoVars, combineMonos, removeMonoVars, filterMonoVars, unify) where

import Tandoori
import Tandoori.State    
import Tandoori.Ty.Unify
import Control.Monad.State
import Language.Haskell.Syntax
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
    
newtype MonoEnv = MonoEnv (Map.Map VarName HsType) deriving Show

emptyMono :: MonoEnv
emptyMono = MonoEnv (Map.empty)

justType :: HsType -> (MonoEnv, HsType)
justType t = (emptyMono, t)

typedAs :: VarName -> HsType -> (MonoEnv, HsType)
name `typedAs` t = (addMonoVar emptyMono (name, t), t)
            
addMonoVar :: MonoEnv -> (VarName, HsType) -> MonoEnv
addMonoVar (MonoEnv m) (name, ty) = MonoEnv (Map.insert name ty m)

(|+|) = addMonoVar                                    
                                    
getMonoVar :: MonoEnv -> VarName -> Maybe HsType
getMonoVar (MonoEnv m) name = Map.lookup name m

(|->|) = getMonoVar                              
                              
monoVars :: MonoEnv -> [(VarName, HsType)]
monoVars (MonoEnv m) = Map.toList m

combineMono :: MonoEnv -> MonoEnv -> MonoEnv
combineMono (MonoEnv m) (MonoEnv m') = MonoEnv $ Map.union m m'

combineMonos :: [MonoEnv] -> MonoEnv
combineMonos ms = foldl combineMono emptyMono ms

removeMonoVars :: MonoEnv -> [VarName] -> MonoEnv
removeMonoVars (MonoEnv m) names = MonoEnv $ foldl removeMonoVar m names
    where removeMonoVar = flip Map.delete

filterMonoVars :: MonoEnv -> (VarName -> HsType -> Bool) -> MonoEnv
filterMonoVars (MonoEnv m) p = MonoEnv $ Map.filterWithKey p m 
                          
unify :: [MonoEnv] -> [HsType] -> Stateful (MonoEnv, HsType)
unify ms tys = do eqs <- monoeqs
                  alpha <- createTv
                  let eqs' = map (\ ty -> (alpha, ty)) tys
                  case mgu (eqs ++ eqs') of
                    Nothing -> error "Unification failed"
                    Just subst -> return (combineMonos (map (substMono subst) ms), substTy subst alpha)
                                  
    where monoeqs = do let vars = concat $ map monoVars ms
                           varnames = distinct $ map fst vars
                       tyvarmap <- liftM Map.fromList $ mapM (\ var -> do tv <- createTv; return (var, tv)) varnames
                       return $ map (\ (var, ty) -> (fromJust (Map.lookup var tyvarmap), ty)) vars                              
          substMono subst (MonoEnv m) = MonoEnv $ Map.map (substTy subst) m
          distinct = Set.toList . Set.fromList
