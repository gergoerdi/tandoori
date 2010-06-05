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


                                
ensuresPredicates :: [LHsPred Name]    -- Predicates which must be ensured by...                     
                     -> CanonizedType  -- ... this type declaration
                     -> Typing Bool
-- ensuresPredicates lpreds  cty = trace (unlines [show predsOut, show predsIn]) $ predsOut `isSubsetOf` predsIn
ensuresPredicates lpreds  cty = do predsIn <- liftM (map unLoc) $ flattenPredsIn (ctyLPreds cty)
                                   predsOut <- liftM (map unLoc) $ flattenPredsOut lpreds
                                   return $ predsOut `isSubsetOf` predsIn
    where xs `isSubsetOf` ys = all (flip elem ys) xs

flattenPredsIn :: [LHsPred Name] -> Typing [LHsPred Name]
flattenPredsIn preds = liftM (distinct . concat) $ mapM flattenPredIn preds
                        
flattenPredIn :: LHsPred Name -> Typing [LHsPred Name]
flattenPredIn (L loc (HsClassP cls [lty])) = do baseClss <- askBaseClassesOf cls                                                    
                                                lpreds' <- flattenPredsIn (map toPred baseClss ++ lpreds)                                                
                                                return $ (L loc (HsClassP cls [lty'])):lpreds'
    where cty = canonize $ unLoc lty
          lpreds = ctyLPreds cty
          toPred cls = noLoc $ HsClassP cls [lty']
          lty' = noLoc $ ctyTy cty
                          
resolvePreds :: CanonizedType -> Typing CanonizedType
resolvePreds cty = do lpreds' <- flattenPredsOut lpreds
                      lpreds'' <- filterM (checkPred . unLoc) lpreds'
                      return $ mkCanonizedType ty lpreds''
    where ty = ctyTy cty
          lpreds = ctyLPreds cty
          checkPred pred = if not (occursInTy pred)
                           then do addError $ AmbiguousPredicate ty pred
                                   return True
                           else return True
          occursInTy (HsClassP cls [lty]) = any (flip occurs ty) $ Set.toList $ tyVarsOf $ unLoc lty
                                            
flattenPredsOut :: [LHsPred Name] -> Typing [LHsPred Name]
flattenPredsOut lpreds = liftM (distinct . concat) $ mapM flattenPredOut lpreds

flattenPredOut :: LHsPred Name -> Typing [LHsPred Name]
flattenPredOut lpred = liftM (map noLoc) $ flatten (unLoc lpred)
    where flatten :: HsPred Name -> Typing [HsPred Name]
          flatten pred@(HsClassP _ [(L _ ty)]) | isTyVar ty = return [pred]
          flatten pred = do directBases <- askBaseInstancesOf pred
                            case directBases of
                              Nothing -> do addError $ UnfulfilledPredicate pred
                                            return []
                              Just preds -> liftM concat $ mapM flatten $ preds
