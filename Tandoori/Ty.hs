module Tandoori.Ty where

import Tandoori
import Tandoori.Util
import Tandoori.GHC.Internals
    
import qualified Data.Set as Set
    
data CanonizedType = CanonizedType { ctyTy :: HsType Name,
                                     ctyLPreds :: HsContext Name }    
    
mkCanonizedType :: HsType Name -> HsContext Name -> CanonizedType
mkCanonizedType ty lpreds = CanonizedType { ctyTy = ty, ctyLPreds = lpreds}

noPreds :: HsType Name -> CanonizedType
noPreds ty = mkCanonizedType ty []
                 
--- Utility
tyCurryCon :: [CanonizedType] -> CanonizedType
tyCurryCon ctys = mkCanonizedType (tyCurryCon' $ map ctyTy ctys) (concatMap ctyLPreds ctys)
    where tyCurryCon' [ty] = ty
          tyCurryCon' (ty:tys) = HsAppTy (noLoc ty) (noLoc $ tyCurryCon' tys)

tyCurryFun :: [CanonizedType] -> CanonizedType
tyCurryFun [cty]      = cty
tyCurryFun (cty:ctys) = mkCanonizedType (HsFunTy (noLoc tyLeft) (noLoc tyRight)) (lpredsLeft ++ lpredsRight)
    where ctyRight = tyCurryFun ctys
          tyLeft = ctyTy cty
          tyRight = ctyTy ctyRight
          lpredsLeft = ctyLPreds cty
          lpredsRight = ctyLPreds ctyRight                                         
    

--- Builtin types                
builtinTyNames = [boolTyConName, intTyConName, charTyConName, listTyConName]
-- builtinTyNames = [intTyConName, charTyConName, listTyConName]

isTyCon :: HsType Name -> Bool                             
isTyCon (HsTyVar name) = (isTyConName name) || (elem name builtinTyNames)
isTyCon _              = False
                             
tyBool       = noPreds $ HsTyVar boolTyConName
tyInt        = noPreds $ HsTyVar intTyConName
tyChar       = noPreds $ HsTyVar charTyConName
tyString     = tyList tyChar
tyList cty   = mkCanonizedType (HsListTy $ noLoc ty) preds
    where ty = ctyTy cty
          preds = ctyLPreds cty                  
tyTuple ctys  = mkCanonizedType (HsTupleTy Boxed $ map noLoc tys) preds
    where tys = map ctyTy ctys
          preds = concatMap ctyLPreds ctys

--- Builtin data constructors
-- builtinDataCons = [nilDataCon, consDataCon]
builtinDataCons = [nilDataCon, consDataCon, trueDataCon, falseDataCon]
builtinDataConNames = map dataConName builtinDataCons

--- Builtin typeclasses
builtinClassNames = [eqClassName, ordClassName, numClassName, fractionalClassName]
                      
--- Types of literals              
typeOfLit :: HsLit -> CanonizedType
typeOfLit (HsInt _)    = tyInt
typeOfLit (HsChar _)   = tyChar
typeOfLit (HsString _) = tyString

tyVarsOf :: HsType Name -> Set.Set TvName
tyVarsOf ty                               | isTyCon ty = Set.empty
tyVarsOf (HsTyVar name)                                = Set.singleton name
tyVarsOf (HsFunTy (L _ left) (L _ right))              = (tyVarsOf left) `Set.union` (tyVarsOf right)
tyVarsOf (HsAppTy (L _ left) (L _ right))              = (tyVarsOf left) `Set.union` (tyVarsOf right)
tyVarsOf (HsTupleTy _ ltys)                            = Set.unions $ map (tyVarsOf . unLoc) ltys
tyVarsOf (HsListTy (L _ tyElem))                       = tyVarsOf tyElem
tyVarsOf (HsForAllTy _ _ _ (L _ ty))                   = tyVarsOf ty
tyVarsOf (HsBangTy _ (L _ ty))                         = tyVarsOf ty
tyVarsOf (HsParTy (L _ ty))                            = tyVarsOf ty

tyVarsOfPred :: HsPred Name -> Set.Set TvName
tyVarsOfPred (HsClassP cls [lty]) = tyVarsOf $ unLoc lty
                                                         
--- Occurs checking                                                         
occurs :: TvName -> TanType -> Bool
occurs x (HsTyVar y)                      = x == y
occurs x (HsFunTy (L _ left) (L _ right)) = (occurs x left) || (occurs x right)
occurs x (HsAppTy (L _ left) (L _ right)) = (occurs x left) || (occurs x right)
occurs x (HsTupleTy _ ltys)               = or $ map (occurs x . unLoc) ltys
occurs x (HsListTy (L _ tyElem))          = occurs x tyElem
occurs x (HsBangTy _ (L _ ty))            = occurs x ty
occurs x (HsForAllTy _ _ _ (L _ ty))      = occurs x ty
occurs x _                                = False
