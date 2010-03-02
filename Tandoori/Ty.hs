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
tyCurryCon :: [TanType] -> TanType -- TODO: CanonizedType
tyCurryCon [ty]     = ty
tyCurryCon (ty:tys) = HsAppTy (noLoc ty) (noLoc $ tyCurryCon tys)

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

isTyCon :: TanType -> Bool                             
isTyCon (HsTyVar name) = (isTyConName name) || (elem name builtinTyNames)
isTyCon _              = False
                             
tyBool       = HsTyVar boolTyConName
tyInt        = HsTyVar intTyConName
tyChar       = HsTyVar charTyConName
tyString     = tyList tyChar
tyList ty    = HsListTy $ noLoc ty
tyTuple tys  = HsTupleTy Boxed $ map noLoc tys

--- Builtin data constructors
builtinDataCons = [nilDataCon, consDataCon] --, trueDataCon, falseDataCon]
builtinDataConNames = map dataConName builtinDataCons

--- Builtin typeclasses
builtinClassNames = [eqClassName, ordClassName, numClassName, fractionalClassName]
                      
--- Types of literals              
typeOfLit :: HsLit -> CanonizedType
typeOfLit (HsInt _)    = noPreds tyInt
typeOfLit (HsChar _)   = noPreds tyChar
typeOfLit (HsString _) = noPreds tyString

tyVarsOf :: TanType -> Set.Set TvName
tyVarsOf ty                               | isTyCon ty = Set.empty
tyVarsOf (HsTyVar name)                                = Set.singleton name
tyVarsOf (HsFunTy (L _ left) (L _ right))              = (tyVarsOf left) `Set.union` (tyVarsOf right)
tyVarsOf (HsAppTy (L _ left) (L _ right))              = (tyVarsOf left) `Set.union` (tyVarsOf right)
tyVarsOf (HsTupleTy _ ltys)                            = Set.unions $ map (tyVarsOf . unLoc) ltys
tyVarsOf (HsListTy (L _ tyElem))                       = tyVarsOf tyElem
tyVarsOf (HsForAllTy _ _ _ (L _ ty))                   = tyVarsOf ty
tyVarsOf (HsBangTy _ (L _ ty))                         = tyVarsOf ty
tyVarsOf (HsParTy (L _ ty))                            = tyVarsOf ty

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
