module Tandoori.Typing.DataType (constructorsFromDecl) where

import Tandoori
import Tandoori.Typing
import Tandoori.Typing.Monad
import Tandoori.GHC.Internals
import Control.Monad
import Control.Monad.Error
    
constructorsFromDecl :: TyClDecl Name -> Typing [(ConName, Ty)]
constructorsFromDecl decl | isDataDecl decl  = do
  let nameData = tcdName decl
      αs = hsTyVarNames $ map unLoc $ tcdTyVars decl
      τData = tyCurryCon $ (TyCon nameData):(map TyVar αs)

  forM (map unLoc $ tcdCons decl) $ \ con -> do
    let tys = map unLoc $ hsConDeclArgTys $ con_details con
    σArgs <- mapM fromHsType tys
    τArgs <- forM σArgs $ \ σ -> do
              case σ of
                PolyTy [] τ -> return τ
                PolyTy ctx τ -> throwErrorLOFASZ $ strMsg "Ctxt in constructor" -- TODO: errors
    let τ = tyCurryFun (τArgs ++ [τData])
    return (unLoc $ con_name con, τ)
constructorsFromDecl _ = return []
