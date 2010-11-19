-- module Tandoori.Typing.UnifyPred (subst, resolvePred, satisfies) where
module Tandoori.Typing.UnifyPred where

import Tandoori.Typing
import Tandoori.Typing.Monad
import Tandoori.Typing.Error
import Tandoori.Typing.Unify
import Tandoori.Typing.Substitute
import Tandoori.Typing.MonoEnv    

import MonadUtils (anyM)
import Control.Monad.Error
import Control.Applicative
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)

substMono θ m = mapMonoM' (return . substTy θ) (substPreds θ) m

substPred :: Subst -> PolyPred -> Typing [PolyPred]
substPred θ (cls, α) = resolvePred (cls, substTy θ (TyVar α))

substPreds :: Subst -> Set PolyPred -> Typing (Set PolyPred)
substPreds θ ctx = 
  do πs <- concat <$> (mapM (substPred θ) $ Set.toList ctx)
     Set.fromList <$> simplifyCtx πs

substCtx :: Subst -> PolyCtx -> Typing PolyCtx
substCtx θ ctx = concat <$> mapM (substPred θ) ctx
          
-- subst θ (PolyTy ctx τ) = do let τ' = substTy θ τ
--                             ctx' <- simplifyCtx =<< substCtx θ ctx     
--                             return $ PolyTy ctx' τ'
                                   
resolvePred :: OverPred -> Typing PolyCtx
resolvePred (cls, τ) = case τ of
                         TyVar α -> return [(cls, α)]
                         τ       -> do let κ = fromJust $ tyCon τ
                                       instData <- askInstance cls κ
                                       case instData of
                                         Nothing -> raiseError $ UnfulfilledPredicate (cls, τ)
                                         Just (PolyTy ctx τ') -> do Right θ <- runErrorT $ fitDeclTy τ τ'
                                                                    substCtx θ ctx

simplifyCtx :: PolyCtx -> Typing PolyCtx
simplifyCtx [] = return []
simplifyCtx (π:πs) = do isRedundant <- anyM (π `isSuperOf`) πs
                        πs' <- filterM (`isNotSuperOf` π) πs
                        if isRedundant then simplifyCtx πs
                           else (π:) <$> simplifyCtx πs'
              where π `isNotSuperOf` π' = not <$> (π `isSuperOf` π')

isSuperOf :: PolyPred -> PolyPred -> Typing Bool
(cls, α) `isSuperOf` (cls', α') | α /= α'    = return False
                                | otherwise  = do supers <- askSupers cls'
                                                  return $ cls `elem` supers
                              
satisfies :: PolyCtx -> PolyCtx -> Typing Bool
general `satisfies` specific = and <$> mapM hasSuper specific
    where hasSuper π = or <$> mapM (π `isSuperOf`) general
                                                                             
