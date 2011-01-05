module Tandoori.Typing.Repr (fromHsType) where

import Tandoori.Typing
import Tandoori.Typing.Monad
import Tandoori.Typing.Error
import Tandoori.GHC.Internals as GHC
    
import qualified TyCon as GHC
import qualified TypeRep as GHC

import Control.Applicative
import Control.Monad.Writer (lift, tell, WriterT, runWriterT)

    
fromHsType :: GHC.HsType GHC.Name -> Typing PolyTy
fromHsType ty = do (τ, ctx) <- runWriterT $ fromHsType' ty
                   return $ PolyTy ctx τ

fromHsType' :: GHC.HsType GHC.Name -> WriterT PolyCtx Typing Ty
fromHsType' τ@(GHC.HsTyVar name) | isTyCon τ  = return $
                                                  case GHC.wiredInNameTyThing_maybe name of
                                                    Just (GHC.ATyCon tycon) -> if GHC.isTupleTyCon tycon then TyTuple (GHC.tyConArity tycon)
                                                                               else TyCon name
                                                    Nothing -> TyCon name
                                 | isTyVar τ  = return $ TyVar name
fromHsType' (GHC.HsFunTy lty1 lty2)           = do τ1 <- fromHsType' $ unLoc lty1
                                                   τ2 <- fromHsType' $ unLoc lty2
                                                   return $ TyFun τ1 τ2
fromHsType' (GHC.HsAppTy lty1 lty2)           = do τ1 <- fromHsType' $ unLoc lty1
                                                   τ2 <- fromHsType' $ unLoc lty2
                                                   return $ TyApp τ1 τ2
fromHsType' (GHC.HsListTy lty)                = tyList <$> fromHsType' (unLoc lty)
fromHsType' (GHC.HsTupleTy _ ltys)            = tyTuple <$> mapM (fromHsType' . unLoc) ltys
fromHsType' (GHC.HsParTy lty)                 = fromHsType' (unLoc lty)
fromHsType' (GHC.HsDocTy lty _)               = fromHsType' (unLoc lty)
fromHsType' (GHC.HsOpTy lty1 (L _ op) lty2)   = do τ1 <- fromHsType' $ unLoc lty1
                                                   τ' <- fromHsType' $ GHC.HsTyVar op
                                                   τ2 <- fromHsType' $ unLoc lty2
                                                   return $ TyApp (TyApp τ' τ1) τ2
fromHsType' (GHC.HsForAllTy _ _ lctxt lty)    = do tell =<< mapM (toPolyPred . unLoc) (unLoc lctxt)
                                                   fromHsType' (unLoc lty)
    where toPolyPred (GHC.HsClassP cls [L _ τ@(GHC.HsTyVar tv)]) | isTyVar τ = return (cls, tv)
          toPolyPred pred@(GHC.HsClassP cls [L _ τ]) = lift $ raiseError $ OtherError "Predicates should only have type variables for parameters"
          toPolyPred pred@(GHC.HsClassP cls _)       = lift $ raiseError $ OtherError "Predicate with more than one type parameter"
          toPolyPred pred                            = lift $ raiseError $ OtherError "Unsupported predicate"
                                                 
                                             
                                  
