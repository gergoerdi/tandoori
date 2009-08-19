module Tandoori.Ty where

import Language.Haskell.Syntax

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
