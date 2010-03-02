module Tandoori.Ty.Canonize (canonize, uncanonize, addPred) where

import Tandoori
import Tandoori.Util
import Tandoori.Ty
import Tandoori.GHC.Internals
import qualified Data.Set as Set

canonize :: HsType Name -> CanonizedType
canonize ty = let (ty', lpreds) = collectPredsTy ty
              in CanonizedType { ctyTy = ty', ctyLPreds = lpreds }

uncanonize :: CanonizedType -> HsType Name
uncanonize cty = HsForAllTy Implicit noBinder lctxt (genLoc ty)
    where lctxt = genLoc $ ctyLPreds cty
          ty = ctyTy cty

addPred :: CanonizedType -> LHsPred Name -> CanonizedType
addPred cty lpred = CanonizedType { ctyTy = ctyTy cty, ctyLPreds = lpred:ctyLPreds cty }
                 
combinePreds :: [HsContext Name] -> HsContext Name
combinePreds [res] = res
combinePreds (preds:predss) = let preds' = combinePreds predss
                              in (preds ++ preds')
                     
collectPredsLTy :: (Located TanType) -> (Located TanType, [LHsPred Name])
collectPredsLTy (L srcloc ty) = let (ty', res) = collectPredsTy ty
                            in (L srcloc ty', res)
                                
liftCollectPredsTy :: (Located TanType -> TanType) -> Located TanType -> (TanType, HsContext Name)
liftCollectPredsTy f lty = let (lty', res) = collectPredsLTy lty
                           in (f lty', res)

collectPredsTy :: TanType -> (TanType, HsContext Name)
collectPredsTy ty@(HsTyVar tv)            = (ty, [])
collectPredsTy (HsFunTy lty1 lty2)        = let (lty1', preds1) = collectPredsLTy lty1
                                                (lty2', preds2) = collectPredsLTy lty2
                                                res = combinePreds [preds1, preds2]
                                            in (HsFunTy lty1' lty2', res)
collectPredsTy (HsAppTy lty1 lty2)        = let (lty1', preds1) = collectPredsLTy lty1
                                                (lty2', preds2) = collectPredsLTy lty2
                                                res = combinePreds [preds1, preds2]
                                            in (HsAppTy lty1' lty2', res)
collectPredsTy (HsListTy lty)             = liftCollectPredsTy HsListTy lty
collectPredsTy (HsTupleTy b ltys)         = let (ltys', predss) = unzip $ map collectPredsLTy ltys
                                                res = combinePreds predss
                                            in (HsTupleTy b ltys', res)
collectPredsTy (HsParTy lty)              = let (lty', preds) = collectPredsLTy lty
                                            in (unLoc lty', preds)
collectPredsTy (HsDocTy lty ldoc)         = liftCollectPredsTy (flip HsDocTy ldoc) lty
collectPredsTy (HsBangTy bang lty)        = liftCollectPredsTy (HsBangTy bang) lty
collectPredsTy (HsForAllTy _ _ lctxt lty) = let (lty', preds') = collectPredsLTy lty
                                                preds = unLoc lctxt
                                            in (unLoc lty', combinePreds [preds, preds'])
                                           
