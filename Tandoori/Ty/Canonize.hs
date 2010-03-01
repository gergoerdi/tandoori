module Tandoori.Ty.Canonize (collectPredsTy, collectPredsLTy) where

import Tandoori
import Tandoori.Util
import Tandoori.Ty
import Tandoori.State
import Tandoori.GHC.Internals
import qualified Data.Set as Set
    
type CanonizationRes = [HsPred Name]

combineRes :: [CanonizationRes] -> CanonizationRes
combineRes [res] = res
combineRes (preds:ress) = let preds' = combineRes ress
                          in (preds ++ preds')
                     
collectPredsLTy :: (Located TanType) -> (Located TanType, CanonizationRes)
collectPredsLTy (L srcloc ty) = let (ty', res) = collectPredsTy ty
                            in (L srcloc ty', res)
                                
liftCollectPredsTy :: (Located TanType -> TanType) -> Located TanType -> (TanType, CanonizationRes)
liftCollectPredsTy f lty = let (lty', res) = collectPredsLTy lty
                       in (f lty', res)

collectPredsTy :: TanType -> (TanType, CanonizationRes)
collectPredsTy ty@(HsTyVar tv)            = (ty, [])
collectPredsTy (HsFunTy lty1 lty2)        = let (lty1', res1) = collectPredsLTy lty1
                                                (lty2', res2) = collectPredsLTy lty2
                                                res = combineRes [res1, res2]
                                            in (HsFunTy lty1' lty2', res)
collectPredsTy (HsAppTy lty1 lty2)        = let (lty1', res1) = collectPredsLTy lty1
                                                (lty2', res2) = collectPredsLTy lty2
                                                res = combineRes [res1, res2]
                                            in (HsAppTy lty1' lty2', res)
collectPredsTy (HsListTy lty)             = liftCollectPredsTy HsListTy lty
collectPredsTy (HsTupleTy b ltys)         = let (ltys', ress) = unzip $ map collectPredsLTy ltys
                                                res = combineRes ress
                                            in (HsTupleTy b ltys', res)
collectPredsTy (HsParTy lty)              = liftCollectPredsTy HsParTy lty
collectPredsTy (HsDocTy lty ldoc)         = liftCollectPredsTy (flip HsDocTy ldoc) lty
collectPredsTy (HsBangTy bang lty)        = liftCollectPredsTy (HsBangTy bang) lty
collectPredsTy (HsForAllTy _ _ lctxt lty) = let (lty', res') = collectPredsLTy lty
                                                res = (map unLoc $ unLoc lctxt)
                                            in (unLoc lty', combineRes [res, res'])
                                           
