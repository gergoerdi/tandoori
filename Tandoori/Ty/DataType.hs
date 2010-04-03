module Tandoori.Ty.DataType (constructorsFromDecl) where

import Tandoori
import Tandoori.Ty   
import Tandoori.Ty.Canonize
import Tandoori.GHC.Internals
    
    
constructorsFromDecl :: TyClDecl Name -> [(ConName, CanonizedType)]
constructorsFromDecl decl | isDataDecl decl  = map (\ con -> (unLoc $ con_name con, tyCon con)) condecls
                          where nameData = tcdName decl 
                                tyvars = map HsTyVar $ hsTyVarNames $ map unLoc $ tcdTyVars decl
                                tyData = tyCurryCon $ map noPreds $ (HsTyVar nameData):tyvars
                                condecls = map unLoc $ tcdCons decl
                                tysArg con = map (noPreds . unLoc) $ hsConDeclArgTys $ con_details con
                                tyCon con = tyCurryFun $ (tysArg con) ++ [tyData]
constructorsFromDecl _                       = []
