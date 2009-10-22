module Tandoori.Ty.DataType (constructorsFromDecl) where

import HsDecls
import HsTypes
import Name
import SrcLoc
    
import Tandoori
import Tandoori.Ty
    
constructorsFromDecl :: TyClDecl Name -> [(ConName, TanType)]
constructorsFromDecl decl | isDataDecl decl = map (\ con -> (unLoc $ con_name con, tyCon con)) condecls
                              where nameData = tcdName decl 
                                    tyvars = map HsTyVar $ hsTyVarNames $ map unLoc $ tcdTyVars decl
                                    tyData = tyCurryCon $ (HsTyVar nameData):tyvars
                                    condecls = map unLoc $ tcdCons decl
                                    tysArg con = map unLoc $ hsConDeclArgTys $ con_details con
                                    tyCon con = tyCurryFun $ (tysArg con) ++ [tyData]
