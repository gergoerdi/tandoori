module Tandoori.Ty.Predicates (resolvePreds, ensuresPredicates) where

import Tandoori.Ty.Canonize
import Tandoori
import Tandoori.Util
import Tandoori.Ty
import Tandoori.Ty.Ctxt
import Tandoori.State
import Tandoori.Errors
import Tandoori.GHC.Internals
import Tandoori.Ty.ClassDecl
import qualified Data.Set as Set
import Control.Monad
    
import Tandoori.Kludge.Show
    
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


                                
ensuresPredicates :: Ctxt
                  -> [HsPred Name]  -- Predicates which must be ensured by...                     
                  -> CanonizedType  -- ... this type declaration
                  -> Bool
-- ensuresPredicates c []     _    = True
ensuresPredicates c preds  cty  = preds `isSubsetOf` (map unLoc preds')
    where lpreds = ctyLPreds cty
          preds' = flattenPredsIn c lpreds
          xs `isSubsetOf` ys = all (flip elem ys) xs

flattenPredsIn :: Ctxt -> [LHsPred Name] -> [LHsPred Name]
flattenPredsIn c preds = distinct $ concat $ map (flattenPredIn c) preds
                        
flattenPredIn :: Ctxt -> LHsPred Name -> [LHsPred Name]
flattenPredIn c (L loc (HsClassP cls [lty])) = (L loc (HsClassP cls [lty'])):lpreds'
    where cty = canonize $ unLoc lty
          lpreds = ctyLPreds cty
          lpreds' = flattenPredsIn c (map toPred baseClss ++ lpreds)
          baseClss = baseClassesOf (classinfo c) cls
          toPred cls = noLoc $ HsClassP cls [lty']
          lty' = noLoc $ ctyTy cty
                          
resolvePreds :: Ctxt -> CanonizedType -> Stateful CanonizedType
resolvePreds c cty = do lpreds'' <- filterOccurs
                        return $ mkCanonizedType ty lpreds''
    where ty = ctyTy cty
          lpreds = ctyLPreds cty
          lpreds' = flattenPredsOut lpreds
          filterOccurs = filterM (checkPred . unLoc) lpreds'
          checkPred pred = if not (occursInTy pred)
                           then do addError $ AmbiguousPredicate ty pred
                                   return False
                           else return True
          occursInTy (HsClassP cls [lty]) = any (flip occurs ty) $ Set.toList $ tyVarsOf $ unLoc lty
                                            
flattenPredsOut :: [LHsPred Name] -> [LHsPred Name]
flattenPredsOut lpreds = distinct $ concat $ map flattenPredOut lpreds
    
flattenPredOut :: LHsPred Name -> [LHsPred Name]
flattenPredOut (L loc (HsClassP cls [lty])) = (L loc (HsClassP cls [lty'])):lpreds
    where cty = canonize $ unLoc lty
          lpreds = ctyLPreds cty
          lty' = noLoc $ ctyTy cty
                          
