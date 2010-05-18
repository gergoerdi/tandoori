module Tandoori.Ty.Predicates (resolvePreds, ensuresPredicates) where

import Tandoori.Ty.Canonize
import Tandoori
import Tandoori.Util
import Tandoori.Ty
import Tandoori.Ty.Ctxt
import Tandoori.Ty.State
import Tandoori.Errors
import Tandoori.GHC.Internals
import Tandoori.Ty.ClassDecl
import qualified Data.Set as Set
import Control.Monad
    
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
                  -> [LHsPred Name]  -- Predicates which must be ensured by...                     
                  -> CanonizedType  -- ... this type declaration
                  -> Typing Bool
-- ensuresPredicates c lpreds  cty = trace (unlines [show predsOut, show predsIn]) $ predsOut `isSubsetOf` predsIn
ensuresPredicates c lpreds  cty = do predsIn <- liftM (map unLoc) $ return $ flattenPredsIn c (ctyLPreds cty)
                                     predsOut <- liftM (map unLoc) $ flattenPredsOut c lpreds
                                     return $ predsOut `isSubsetOf` predsIn
    where xs `isSubsetOf` ys = all (flip elem ys) xs

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
                          
resolvePreds :: Ctxt -> CanonizedType -> Typing CanonizedType
resolvePreds c cty = do lpreds' <- flattenPredsOut c lpreds
                        lpreds'' <- filterM (checkPred . unLoc) lpreds'
                        return $ mkCanonizedType ty lpreds''
    where ty = ctyTy cty
          lpreds = ctyLPreds cty
          checkPred pred = if not (occursInTy pred)
                           then do addError $ AmbiguousPredicate ty pred
                                   return True
                           else return True
          occursInTy (HsClassP cls [lty]) = any (flip occurs ty) $ Set.toList $ tyVarsOf $ unLoc lty
                                            
flattenPredsOut :: Ctxt -> [LHsPred Name] -> Typing [LHsPred Name]
flattenPredsOut c lpreds = liftM (distinct . concat) $ mapM (flattenPredOut c) lpreds
    
flattenPredOut :: Ctxt -> LHsPred Name -> Typing [LHsPred Name]
flattenPredOut c (L loc (HsClassP cls [lty])) = return $ lpreds'
    where lpreds' = filter (\ (L _ (HsClassP cls [lty])) -> hasTyVars (unLoc lty)) (lpred:lpreds)
          lpreds = ctyLPreds cty
          lpred = L loc $ HsClassP cls [lty']
          lty' = noLoc $ ctyTy cty
          cty = canonize $ unLoc lty
                
          hasTyVars ty = True
          hasTyVars ty = not $ Set.null $ tyVarsOf ty
                          
