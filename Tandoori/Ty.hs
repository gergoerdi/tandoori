module Tandoori.Ty where

import Tandoori
    
import HsTypes
import BasicTypes
import SrcLoc
import TysWiredIn (intTyConName, charTyConName, boolTyConName, listTyConName, nilDataCon, consDataCon)
import PrelNames (stringTyConName)
import HsLit
import Name (Name, isTyConName)
import DataCon (dataConName)
    
import qualified Data.Set as Set
    
--- Utility                                                      
tyCurryCon :: [TanType] -> TanType
tyCurryCon [ty]     = ty
tyCurryCon (ty:tys) = HsAppTy (noLoc ty) (noLoc $ tyCurryCon tys)

tyCurryFun :: [TanType] -> TanType
tyCurryFun [ty]     = ty
tyCurryFun (ty:tys) = HsFunTy (noLoc ty) (noLoc $ tyCurryFun tys)

tyUncurryFun :: TanType -> [TanType]
tyUncurryFun (HsFunTy left right) = (unLoc left):(tyUncurryFun $ unLoc right)
tyUncurryFun ty                   = [ty]

--- Builtin types                
builtinTyNames = [boolTyConName, intTyConName, charTyConName, listTyConName]

isTyCon :: TanType -> Bool                             
isTyCon (HsTyVar name) = (isTyConName name) || (elem name builtinTyNames)
isTyCon _              = False
                             
tyBool      = HsTyVar boolTyConName
tyInt       = HsTyVar intTyConName
tyChar      = HsTyVar charTyConName
tyString    = tyList tyChar
tyList ty   = HsListTy $ noLoc ty
tyTuple tys = HsTupleTy Boxed $ map noLoc tys

--- Builtin data constructors
builtinDataCons = [nilDataCon, consDataCon]
builtinDataConNames = map dataConName builtinDataCons

--- Types of literals              
typeOfLit :: HsLit -> TanType
typeOfLit (HsInt _)    = tyInt
typeOfLit (HsChar _)   = tyChar
typeOfLit (HsString _) = tyString

typeOfOverLit :: HsOverLit Name -> TanType
typeOfOverLit (OverLit { ol_val = val }) = case val of
                                             HsIntegral _ -> tyInt
                                             HsIsString _ -> tyString
                         
ltyVarsOf :: Located TanType -> Set.Set TvName
ltyVarsOf = tyVarsOf . unLoc
                         
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
                                               
occurs :: TvName -> TanType -> Bool
occurs x (HsTyVar y)                      = x == y
occurs x (HsFunTy (L _ left) (L _ right)) = (occurs x left) || (occurs x right)
occurs x (HsAppTy (L _ left) (L _ right)) = (occurs x left) || (occurs x right)
occurs x (HsTupleTy _ ltys)               = or $ map (occurs x . unLoc) ltys
occurs x (HsListTy (L _ tyElem))          = occurs x tyElem
occurs x (HsBangTy _ (L _ ty))            = occurs x ty
occurs x (HsForAllTy _ _ _ (L _ ty))      = occurs x ty
occurs x _                                = False
