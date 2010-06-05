module Tandoori.Ty.Substitute (Substitution, substCTy, addSubst, emptySubst, substTy) where

import Tandoori
import Tandoori.Ty
import Tandoori.Ty.ShowTy    
import Tandoori.GHC.Internals    
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
    
newtype Substitution = S (Map.Map TvName TanType)

showSubst :: Substitution -> String
showSubst (S s) = show s

instance Show Substitution where
    show = showSubst
                  
emptySubst :: Substitution
emptySubst = S Map.empty

getSubst :: Substitution -> TvName -> Maybe TanType
getSubst (S m) name = Map.lookup name m

addSubst :: Substitution -> TvName -> TanType -> Substitution
addSubst (S m) name ty = S $ Map.insert name ty m

data SubstRes a = SubstRes (Set.Set TvName) a

instance (Monad SubstRes) where
    (SubstRes tvs x) >>= f = SubstRes (tvs `Set.union` tvs') y
        where SubstRes tvs' y = f x
    return x = SubstRes Set.empty x
                
seenTyVar :: Name -> SubstRes ()
seenTyVar tv = SubstRes (Set.singleton tv) ()

substTyM :: Substitution -> (HsType Name) -> SubstRes (HsType Name)
substTyM s ty | isTyCon ty      = return ty
substTyM s ty@(HsTyVar name)    = do seenTyVar name
                                     case getSubst s name of
                                       Nothing -> return ty
                                       Just ty' -> substTyM s ty'
substTyM s (HsFunTy ltyL ltyR)  = do ltyL' <- substLTyM s ltyL
                                     ltyR' <- substLTyM s ltyR
                                     return $ HsFunTy ltyL' ltyR'
substTyM s (HsAppTy ltyL ltyR)  = do ltyL' <- substLTyM s ltyL
                                     ltyR' <- substLTyM s ltyR
                                     return $ HsAppTy ltyL' ltyR'
substTyM s (HsListTy lty)       = liftM HsListTy $ substLTyM s lty
substTyM s (HsTupleTy b ltys)   = liftM (HsTupleTy b) $ mapM (substLTyM s) ltys                                   
substTyM s (HsParTy lty)        = liftM HsParTy $ substLTyM s lty

substTy :: Substitution -> HsType Name -> HsType Name
substTy s ty = let (SubstRes tvs ty') = substTyM s ty
               in ty'
                            
substLTyM :: Substitution -> (Located (HsType Name)) -> SubstRes (Located (HsType Name))
substLTyM s lty = do ty' <- substTyM s (unLoc lty)
                     return $ noLoc ty'

substLTy :: Substitution -> LHsType Name -> LHsType Name
substLTy s lty = let (SubstRes tvs ty') = substLTyM s lty
                 in ty'
                    
substCTy :: Substitution -> (CanonizedType, HsContext Name) -> CanonizedType
substCTy s (cty, ctxt) = mkCanonizedType ty' ctxtInEffect
    where SubstRes tvs ty' = substTyM s ty
          -- ctxtInEffect = trace (show (ctxt, s)) $ (map (substLPred s) $ ctyLPreds cty) ++ (filter hasRelevantTyVars ctxt')
          ctxtInEffect = (map (substLPred s) $ ctyLPreds cty) ++ (filter hasRelevantTyVars ctxt')
          ctxt' = map (substLPred s) ctxt
          ty = ctyTy cty
          -- ctxt'' = map (substLPred s) ctxtInEffect
          -- hasRelevantTyVars lpred = True
          hasRelevantTyVars lpred = any (\ tv -> tv `Set.member` tvs) $ Set.toList $ tyVarsOfPred (unLoc lpred)
                                        
substLPred :: Substitution -> LHsPred Name -> LHsPred Name
substLPred s = noLoc . substPred s . unLoc
                                 
substPred :: Substitution -> HsPred Name -> HsPred Name
substPred s (HsClassP cls [lty]) = HsClassP cls [substLTy s lty]
