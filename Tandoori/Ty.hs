module Tandoori.Ty where

import Tandoori
import Language.Haskell.Syntax
import qualified Data.Set as Set
    
--- Utility                                                      
tyCurryCon :: [HsType] -> HsType
tyCurryCon [ty]     = ty
tyCurryCon (ty:tys) = HsTyApp ty (tyCurryCon tys)

tyCurryFun :: [HsType] -> HsType
tyCurryFun [ty]     = ty
tyCurryFun (ty:tys) = HsTyFun ty (tyCurryFun tys)                    

tyUncurryFun :: HsType -> [HsType]
tyUncurryFun (HsTyFun left right) = left:(tyUncurryFun right)
tyUncurryFun ty                   = [ty]

--- Builtin types                
tyBool      = HsTyCon (UnQual (HsIdent "Bool"))
tyInt       = HsTyCon (UnQual (HsIdent "Int"))
tyChar      = HsTyCon (UnQual (HsIdent "Char"))
tyList ty   = HsTyApp list_tycon ty
tyTuple tys = HsTyTuple tys

--- Types of literals              
typeOfLit :: HsLiteral -> HsType
typeOfLit (HsInt _)    = tyInt
typeOfLit (HsChar _)   = tyChar
typeOfLit (HsString _) = tyList tyChar                   

                         
tyVarsOf :: HsType -> Set.Set HsName
tyVarsOf (HsTyVar name)       = Set.singleton name
tyVarsOf (HsTyCon _)          = Set.empty
tyVarsOf (HsTyFun left right) = (tyVarsOf left) `Set.union` (tyVarsOf right)
tyVarsOf (HsTyApp left right) = (tyVarsOf left) `Set.union` (tyVarsOf right)
tyVarsOf (HsTyTuple tys)      = Set.unions $ map tyVarsOf tys

occurs :: TvName -> HsType -> Bool
occurs x (HsTyVar y)    = x == y
occurs x (HsTyFun t t') = occurs x t || occurs x t'
occurs x (HsTyApp t t') = occurs x t || occurs x t'
occurs x _ = False
