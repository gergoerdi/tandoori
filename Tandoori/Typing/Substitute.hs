module Tandoori.Typing.Substitute where
-- module Tandoori.Ty.Substitute (Substitution, substCTy, addSubst, emptySubst, substTy) where

import Tandoori
import Tandoori.Typing
import Control.Monad
import Control.Monad.RWS
import qualified Data.Map as Map
import qualified Data.Set as Set
    
newtype Subst = S (Map.Map Tv Ty)

emptySubst :: Subst
emptySubst = S Map.empty

getSubst :: Tv -> Subst -> Maybe Ty
getSubst α (S m) = Map.lookup α m

addSubst :: Tv -> Ty -> Subst -> Subst
addSubst α τ (S m) = S $ Map.insert α τ m

substTyM τ@(TyVar α) = do tell $ Set.singleton α
                          lookup <- asks $ getSubst α
                          case lookup of
                            Nothing -> return τ
                            Just τ' -> substTyM τ'
substTyM (TyFun τ1 τ2) = liftM2 TyFun (substTyM τ1) (substTyM τ2)
substTyM (TyApp τ1 τ2) = liftM2 TyApp (substTyM τ1) (substTyM τ2)
substTyM τ@(TyCon _) = return τ
substTyM τ@(TyTuple _) = return τ
                         
substTy :: Subst -> Ty -> Ty
substTy s τ = fst $ evalRWS (substTyM τ) s ()
