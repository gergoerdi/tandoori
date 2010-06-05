module Tandoori.Ty.Canonize (canonize, uncanonize, addPred) where

import Tandoori
import Tandoori.Util
import Tandoori.Ty
import Tandoori.GHC.Internals
import Control.Monad.Writer
import qualified Data.Set as Set

canonize :: HsType Name -> CanonizedType
canonize ty = let (ty', lpreds) = runWriter (collectPredsTy ty)
              in CanonizedType { ctyTy = ty', ctyLPreds = lpreds }

uncanonize :: CanonizedType -> HsType Name
uncanonize cty = HsForAllTy Implicit noBinder lctxt (genLoc ty)
    where lctxt = genLoc $ ctyLPreds cty
          ty = ctyTy cty

addPred :: CanonizedType -> LHsPred Name -> CanonizedType
addPred cty lpred = CanonizedType { ctyTy = ctyTy cty, ctyLPreds = lpred:ctyLPreds cty }
                 
collectPredsLTy :: (Located TanType) -> Writer (HsContext Name) (Located TanType)
collectPredsLTy (L srcloc ty) = liftM (L srcloc) $ collectPredsTy ty
                                
collectPredsTy :: TanType -> Writer (HsContext Name) TanType
collectPredsTy ty@(HsTyVar _)             = return ty
collectPredsTy (HsFunTy lty1 lty2)        = do lty1' <- collectPredsLTy lty1
                                               lty2' <- collectPredsLTy lty2
                                               return $ HsFunTy lty1' lty2'
collectPredsTy (HsAppTy lty1 lty2)        = do lty1' <- collectPredsLTy lty1
                                               lty2' <- collectPredsLTy lty2
                                               return $ HsAppTy lty1' lty2'
collectPredsTy (HsListTy lty)             = liftM HsListTy $ collectPredsLTy lty
collectPredsTy (HsTupleTy b ltys)         = liftM (HsTupleTy b) $ mapM collectPredsLTy ltys
collectPredsTy (HsParTy lty)              = liftM unLoc $ collectPredsLTy lty
collectPredsTy (HsDocTy lty ldoc)         = liftM (flip HsDocTy ldoc) $ collectPredsLTy lty
collectPredsTy (HsBangTy bang lty)        = liftM (HsBangTy bang) $ collectPredsLTy lty
collectPredsTy (HsForAllTy _ _ lctxt lty) = do tell $ unLoc lctxt
                                               liftM unLoc $ collectPredsLTy lty
collectPredsTy (HsPredTy (HsClassP cls [lty])) = do lty' <- collectPredsLTy lty
                                                    return $ HsPredTy $ HsClassP cls [lty']
