module Tandoori.Ty.Predicates (resolvePreds, ensuresPredicates) where

import Tandoori.Ty.Canonize
import Tandoori
import Tandoori.Util
import Tandoori.Ty
import Tandoori.Ty.Ctxt
import Tandoori.State
import Tandoori.GHC.Internals
import Tandoori.Ty.ClassDecl
import qualified Data.Set as Set

import Tandoori.Kludge.Show
    
type CanonizationRes = [HsPred Name]

-- simplifyPred :: Ctxt -> [HsPred Name] -> HsPred Name -> ([HsPred Name], [HsPred Name])
-- simplifyPred c ps p                      | elem p ps  = ([], [])
-- simplifyPred c ps p@(HsClassP cls [lty])              = case unLoc lty of
--                                                           HsTyVar tv -> p
                                             
                                        
                                            
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
                  -> TanType        -- ... this type declaration
                  -> Bool
ensuresPredicates c []     _                         = True
ensuresPredicates c preds  (HsForAllTy _ _ lctxt _)  = preds `isSubsetOf` preds'
    where preds' = flattenPredsIn c $ map unLoc $ unLoc lctxt
          xs `isSubsetOf` ys = all (flip elem ys) xs
ensuresPredicates c _      _                         = False                                

flattenPredsIn :: Ctxt -> [HsPred Name] -> [HsPred Name]
flattenPredsIn c preds = distinct $ concat $ map (flattenPredIn c) preds
                        
flattenPredIn :: Ctxt -> HsPred Name -> [HsPred Name]
flattenPredIn c (HsClassP cls [lty]) = (HsClassP cls [lty']):preds'
    where (lty', preds) = collectPredsLTy lty
          preds' = flattenPredsIn c (map toPred baseClss ++ preds)
          baseClss = baseClassesOf (classinfo c) cls
          toPred cls = HsClassP cls [lty']
              


                          
resolvePreds :: Ctxt -> TanType -> Stateful TanType
resolvePreds c ty = do preds' <- simplifyPreds $ flattenPredsOut preds
                       return $ tyFromPreds ty' preds'
    where (ty', preds) = collectPredsTy ty
          simplifyPreds preds = return preds -- TODO: check for ambigous constraints
          occursInTy (HsClassP cls [lty]) = any (flip occurs ty) $ Set.toList $ tyVarsOf $ unLoc lty
                                            
flattenPredsOut :: [HsPred Name] -> [HsPred Name]
flattenPredsOut preds = distinct $ concat $ map flattenPredOut preds
    
flattenPredOut :: HsPred Name -> [HsPred Name]
flattenPredOut (HsClassP cls [lty]) = (HsClassP cls [lty']):preds
    where (lty', preds) = collectPredsLTy lty
                          
