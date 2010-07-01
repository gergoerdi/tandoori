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
                         
-- substTy :: Subst -> HsType Name -> HsType Name
-- substTy s ty = let (SubstRes tvs ty') = substTyM s ty
--                in ty'
                            
-- substLTyM :: Subst -> (Located (HsType Name)) -> SubstRes (Located (HsType Name))
-- substLTyM s lty = do ty' <- substTyM s (unLoc lty)
--                      return $ noLoc ty'

-- substLTy :: Subst -> LHsType Name -> LHsType Name
-- substLTy s lty = let (SubstRes tvs ty') = substLTyM s lty
--                  in ty'
                    
-- substCTy :: Subst -> (CanonizedType, HsContext Name) -> CanonizedType
-- substCTy s (cty, ctxt) = mkCanonizedType ty' ctxtInEffect
--     where SubstRes tvs ty' = substTyM s ty
--           -- ctxtInEffect = trace (show (ctxt, s)) $ (map (substLPred s) $ ctyLPreds cty) ++ (filter hasRelevantTyVars ctxt')
--           ctxtInEffect = (map (substLPred s) $ ctyLPreds cty) ++ (filter hasRelevantTyVars ctxt')
--           ctxt' = map (substLPred s) ctxt
--           ty = ctyTy cty
--           -- ctxt'' = map (substLPred s) ctxtInEffect
--           -- hasRelevantTyVars lpred = True
--           hasRelevantTyVars lpred = any (\ tv -> tv `Set.member` tvs) $ Set.toList $ tyVarsOfPred (unLoc lpred)
                                        
-- substLPred :: Subst -> LHsPred Name -> LHsPred Name
-- substLPred s = noLoc . substPred s . unLoc
                                 
-- substPred :: Subst -> HsPred Name -> HsPred Name
-- substPred s (HsClassP cls [lty]) = HsClassP cls [substLTy s lty]
