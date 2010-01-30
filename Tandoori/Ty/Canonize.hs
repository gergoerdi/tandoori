module Tandoori.Ty.Canonize (canonizeTy) where

import Tandoori
import Tandoori.Util
import Tandoori.Ty
import Tandoori.Ty.Ctxt
import Tandoori.State
import Tandoori.GHC.Internals
import qualified Data.Set as Set
    
data CanonizationRes = CanonizationRes [HsPred Name]

isTrivialRes :: CanonizationRes -> Bool
isTrivialRes (CanonizationRes preds) = null preds

forallFromRes :: Ctxt -> CanonizationRes -> TanType -> TanType
forallFromRes c (CanonizationRes preds) ty = HsForAllTy Implicit noBinder lctxt lty
    where lctxt = genLoc (map genLoc preds')
          preds' = filter occursInTy $ distinct $ concat $ map flattenPreds preds
          lty = genLoc ty
          occursInTy (HsClassP cls [lty']) = any (flip occurs ty) $ Set.toList $ tyVarsOf $ unLoc lty'

instance Eq HsBang where
    HsNoBang  ==  HsNoBang  = True
    HsStrict  ==  HsStrict  = True
    HsUnbox   ==  HsUnbox   = True
    
instance Eq name => Eq (HsType name) where
    (HsTyVar tv)            ==  (HsTyVar tv')              = tv == tv'
    (HsBangTy b lty)        ==  (HsBangTy b' lty')         = b == b' && lty == lty'
    (HsAppTy lty1 lty2)     ==  (HsAppTy lty1' lty2')      = lty1 == lty1' && lty2 == lty2'
    (HsFunTy lty1 lty2)     ==  (HsFunTy lty1' lty2')      = lty1 == lty1' && lty2 == lty2'
    (HsListTy lty)          ==  (HsListTy lty')            = lty == lty'
    (HsTupleTy b ltys)      ==  (HsTupleTy b' ltys')       = b == b' && ltys == ltys'

    (HsParTy lty)           ==  ty'                        = (unLoc lty) == ty'
    ty                      ==  (HsParTy lty')             = ty == (unLoc lty')
                                                        
    (HsOpTy lty1 lop lty2)  ==  ty'                        = HsAppTy (noLoc $ HsAppTy (noLoc $ HsTyVar (unLoc lop)) lty1) lty2 == ty'
    ty                      ==  (HsOpTy lty1' lop' lty2')  = ty == HsAppTy (noLoc $ HsAppTy (noLoc $ HsTyVar (unLoc lop')) lty1') lty2'

    _                       ==  _                          = False
                                                             
instance Eq name => Eq (HsPred name) where
    (HsClassP cls [lty]) == (HsClassP cls' [lty']) = (cls == cls') && (lty == lty')

instance Eq a => Eq (Located a) where
    (L _ x) == (L _ y) = x == y                                              
                
distinct []                   = []
distinct (x:xs)  | elem x xs  = distinct xs
                 | otherwise  = x:distinct xs

flattenPreds :: HsPred Name -> [HsPred Name]
flattenPreds (HsClassP cls [lty]) = (HsClassP cls [lty']):preds
    where (lty', CanonizationRes preds) = canonizeLTy lty
                                           
combineRes :: [CanonizationRes] -> CanonizationRes
combineRes [res] = res
combineRes ((CanonizationRes preds):ress) = let CanonizationRes preds' = combineRes ress
                                            in CanonizationRes (preds ++ preds')
                     
canonizeTy :: Ctxt -> TanType -> Stateful TanType
canonizeTy c ty | isTrivialRes res = return ty'
                | otherwise = return $ forallFromRes c res ty'
    where (ty', res) = canonizeTy' ty
                       
canonizeLTy :: (Located TanType) -> (Located TanType, CanonizationRes)
canonizeLTy (L srcloc ty) = let (ty', res) = canonizeTy' ty
                            in (L srcloc ty', res)
                                
liftCanonizeTy :: (Located TanType -> TanType) -> Located TanType -> (TanType, CanonizationRes)
liftCanonizeTy f lty = let (lty', res) = canonizeLTy lty
                       in (f lty', res)

canonizeTy' :: TanType -> (TanType, CanonizationRes)
canonizeTy' ty@(HsTyVar tv)            = (ty, CanonizationRes [])
canonizeTy' (HsFunTy lty1 lty2)        = let (lty1', res1) = canonizeLTy lty1
                                             (lty2', res2) = canonizeLTy lty2
                                             res = combineRes [res1, res2]
                                         in (HsFunTy lty1' lty2', res)
canonizeTy' (HsAppTy lty1 lty2)        = let (lty1', res1) = canonizeLTy lty1
                                             (lty2', res2) = canonizeLTy lty2
                                             res = combineRes [res1, res2]
                                         in (HsAppTy lty1' lty2', res)
canonizeTy' (HsListTy lty)             = liftCanonizeTy HsListTy lty
canonizeTy' (HsTupleTy b ltys)         = let (ltys', ress) = unzip $ map canonizeLTy ltys
                                             res = combineRes ress
                                         in (HsTupleTy b ltys', res)
canonizeTy' (HsParTy lty)              = liftCanonizeTy HsParTy lty
canonizeTy' (HsDocTy lty ldoc)         = liftCanonizeTy (flip HsDocTy ldoc) lty
canonizeTy' (HsBangTy bang lty)        = liftCanonizeTy (HsBangTy bang) lty
canonizeTy' (HsForAllTy _ _ lctxt lty) = let (lty', res') = canonizeLTy lty
                                             res = CanonizationRes (map unLoc $ unLoc lctxt)
                                         in (unLoc lty', combineRes [res, res'])                                             
