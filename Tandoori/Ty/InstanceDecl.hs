module Tandoori.Ty.InstanceDecl where

import Tandoori
import Tandoori.Ty   
import Tandoori.Errors
import Tandoori.GHC.Internals
import Tandoori.Ty.Canonize
import Tandoori.Ty.Unify
import Tandoori.Ty.Substitute
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map

data TyConKey = TyConKeyList
              | TyConKeyTuple Int
              | TyConKeyData TvName
              deriving (Eq, Ord)
    
data InstanceDesc = InstanceDesc { cls :: Name, tyCon :: TyConKey }
                  deriving (Eq, Ord)

type InstanceMap = Map.Map InstanceDesc CanonizedType                           
                           
mkInstanceMap :: [LInstDecl Name] -> InstanceMap
mkInstanceMap ldecls = Map.fromList $ map (toPair . unLoc) ldecls
    where toPair (InstDecl (L _ ty) _ _ _) = (toInstanceDesc pred, cty)
              where cty@(CanonizedType (HsPredTy pred) lpreds) = canonize ty

bases :: InstanceMap -> HsPred Name -> Maybe [HsPred Name]
bases instmap pred@(HsClassP cls [lty]) = do cty <- Map.lookup desc instmap
                                             let HsPredTy (HsClassP _ [lty']) = ctyTy cty
                                                 Right s = mgu [unLoc lty' :=: unLoc lty]
                                                 cty' = substCTy s (cty, [])
                                             return $ map unLoc $ ctyLPreds cty'
    where desc = toInstanceDesc pred

baseInstancesOf :: InstanceMap -> HsPred Name -> Maybe [HsPred Name]                 
baseInstancesOf instmap pred = do directBases <- bases instmap pred
                                  liftM concat $ mapM (baseInstancesOf instmap) directBases
                 
toInstanceDesc :: HsPred Name -> InstanceDesc
toInstanceDesc (HsClassP cls [lty]) = InstanceDesc cls (getTyConKey $ unLoc lty)

getTyConKey :: HsType Name -> TyConKey
getTyConKey (HsAppTy (L _ t) _) = getTyConKey t
getTyConKey (HsListTy _) = TyConKeyList
getTyConKey (HsTupleTy _ ltys) = TyConKeyTuple (length ltys)
getTyConKey (HsTyVar tv) = TyConKeyData tv
