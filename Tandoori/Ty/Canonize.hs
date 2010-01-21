module Tandoori.Ty.Canonize (canonizeTy) where

import Tandoori
import Tandoori.GHC.Internals
    
data CanonizationRes = CanonizationRes [HsTyVarBndr Name] [HsPred Name]

isTrivialRes :: CanonizationRes -> Bool
isTrivialRes (CanonizationRes [] _) = True
isTrivialRes _                      = False

forallFromRes :: CanonizationRes -> TanType -> TanType
forallFromRes (CanonizationRes bndrs preds) ty = HsForAllTy Implicit (map genLoc bndrs) (genLoc (map genLoc preds)) (genLoc ty)
                                      
combineRes :: [CanonizationRes] -> CanonizationRes
combineRes [res] = res
combineRes ((CanonizationRes bndrs preds):ress) = let CanonizationRes bndrs' preds' = combineRes ress
                                                  in CanonizationRes (bndrs ++ bndrs') (preds ++ preds')
                     
canonizeTy :: TanType -> TanType
canonizeTy ty | isTrivialRes res = ty'
              | otherwise = forallFromRes res ty'
    where (ty', res) = canonizeTy' ty
                       
canonizeLTy :: (Located TanType) -> (Located TanType, CanonizationRes)
canonizeLTy (L srcloc ty) = let (ty', res) = canonizeTy' ty
                            in (L srcloc ty', res)
                                
liftCanonizeTy :: (Located TanType -> TanType) -> Located TanType -> (TanType, CanonizationRes)
liftCanonizeTy f lty = let (lty', res) = canonizeLTy lty
                       in (f lty', res)

canonizeTy' :: TanType -> (TanType, CanonizationRes)
canonizeTy' tv@(HsTyVar name)               = (tv, CanonizationRes [] [])
canonizeTy' (HsFunTy lty1 lty2)             = let (lty1', res1) = canonizeLTy lty1
                                                  (lty2', res2) = canonizeLTy lty2
                                                  res = combineRes [res1, res2]
                                              in (HsFunTy lty1' lty2', res)
canonizeTy' (HsAppTy lty1 lty2)             = let (lty1', res1) = canonizeLTy lty1
                                                  (lty2', res2) = canonizeLTy lty2
                                                  res = combineRes [res1, res2]
                                              in (HsAppTy lty1' lty2', res)
canonizeTy' (HsListTy lty)                  = liftCanonizeTy HsListTy lty
canonizeTy' (HsTupleTy b ltys)              = let (ltys', ress) = unzip $ map canonizeLTy ltys
                                                  res = combineRes ress
                                              in (HsTupleTy b ltys', res)
canonizeTy' (HsParTy lty)                   = liftCanonizeTy HsParTy lty
canonizeTy' (HsDocTy lty ldoc)              = liftCanonizeTy (flip HsDocTy ldoc) lty
canonizeTy' (HsBangTy bang lty)             = liftCanonizeTy (HsBangTy bang) lty
canonizeTy' (HsForAllTy _ lbndrs lctxt lty) = let (lty', res') = canonizeLTy lty
                                                  res = CanonizationRes (map unLoc lbndrs) (map unLoc $ unLoc lctxt)
                                              in (unLoc lty', combineRes [res, res'])
