module Tandoori.Typing where

import Tandoori
import Tandoori.Util
import qualified Tandoori.GHC.Internals as GHC
import Tandoori.GHC.Internals (Located(..), unLoc)
    
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Writer
import Data.List (nub)
    
type Tv = GHC.Name    
type Con = GHC.Name
type Cls = GHC.Name
    
data Ty = TyVar Tv
        | TyCon Con
        | TyApp Ty Ty
        | TyFun Ty Ty
          -- TODO: Records
          -- TODO: Show instance

data TyCon = TyConData Con
           | TyConFun
           deriving (Eq, Ord)
          
data TyEq = Ty :=: Ty
          
type OverPred = (Cls, Ty)
type OverCtx = [OverPred]
data OverTy = OverTy OverCtx Ty

type PolyPred = (Cls, Tv)
type PolyCtx = [PolyPred]
data PolyTy = PolyTy PolyCtx Ty

data TypingError = Unsolvable Ty Ty
                 | InfiniteType Ty Ty
                 | OtherError String -- TODO
                 
instance Error TypingError where
    strMsg = OtherError
            
fromHsType :: GHC.HsType GHC.Name -> Either TypingError PolyTy
fromHsType ty = do (τ, ctx) <- runWriterT $ fromHsType' ty
                   return $ PolyTy ctx τ
              
-- fromHsType' :: GHC.HsType GHC.Name -> WriterT PolyCtx (Either TypingError) Ty
fromHsType' τ@(GHC.HsTyVar name) | isTyCon τ  = return $ TyCon name
                                 | isTyVar τ  = return $ TyVar name
fromHsType' (GHC.HsFunTy lty1 lty2)           = do τ1 <- fromHsType' $ unLoc lty1
                                                   τ2 <- fromHsType' $ unLoc lty2
                                                   return $ TyFun τ2 τ2
fromHsType' (GHC.HsAppTy lty1 lty2)           = do τ1 <- fromHsType' $ unLoc lty1
                                                   τ2 <- fromHsType' $ unLoc lty2
                                                   return $ TyApp τ1 τ2
fromHsType' (GHC.HsListTy lty)                = tyList <$> fromHsType' (unLoc lty)
fromHsType' (GHC.HsTupleTy _ ltys)            = error "TODO: tuples"
fromHsType' (GHC.HsParTy lty)                 = fromHsType' (unLoc lty)
fromHsType' (GHC.HsDocTy lty _)               = fromHsType' (unLoc lty)
fromHsType' (GHC.HsOpTy lty1 (L _ op) lty2)   = do τ1 <- fromHsType' $ unLoc lty1
                                                   τ' <- fromHsType' $ GHC.HsTyVar op
                                                   τ2 <- fromHsType' $ unLoc lty2
                                                   return $ TyApp (TyApp τ' τ1) τ2
fromHsType' (GHC.HsForAllTy _ _ lctxt lty)    = do tell =<< mapM (toPolyPred . unLoc) (unLoc lctxt)
                                                   fromHsType' (unLoc lty)
    where toPolyPred (GHC.HsClassP cls [L _ τ@(GHC.HsTyVar tv)]) | isTyVar τ = return (cls, tv)
          toPolyPred (GHC.HsClassP cls [L _ τ]) = throwError $ strMsg "Malformed predicate"
          toPolyPred (GHC.HsClassP cls _)       = throwError $ strMsg "Predicate with more than one type parameter"
          toPolyPred _                          = throwError $ strMsg "Unsupported predicate"
                                                 
                                             
fromPolyCtx ctx = map (fmap TyVar) ctx
fromPolyTy (PolyTy ctx ty) = OverTy (fromPolyCtx ctx) ty
              
--- Utility
tyCurryCon :: [Ty] -> Ty
tyCurryCon τs = foldr1 TyApp τs
                
tyCurryFun :: [Ty] -> Ty
tyCurryFun τs = foldr1 TyFun τs

ptyCurryFun :: [PolyTy] -> PolyTy
ptyCurryFun σs = PolyTy ctx' τ'
    where (ctxs, τs) = unzip $ map (\ (PolyTy ctx τ) -> (ctx, τ)) σs
          ctx' = nub $ concat ctxs
          τ' = tyCurryFun τs
                
-- tyCurryFun :: [CanonizedType] -> CanonizedType
-- tyCurryFun [cty]      = cty
-- tyCurryFun (cty:ctys) = mkCanonizedType (HsFunTy (noLoc tyLeft) (noLoc tyRight)) (lpredsLeft ++ lpredsRight)
--     where ctyRight = tyCurryFun ctys
--           tyLeft = ctyTy cty
--           tyRight = ctyTy ctyRight
--           lpredsLeft = ctyLPreds cty
--           lpredsRight = ctyLPreds ctyRight                                         
    

--- Builtin types                
builtinTyNames = [GHC.boolTyConName, GHC.intTyConName, GHC.charTyConName, GHC.listTyConName]
-- builtinTyNames = [intTyConName, charTyConName, listTyConName]

isTyCon :: GHC.HsType GHC.Name -> Bool                             
isTyCon (GHC.HsTyVar name) = (GHC.isTyConName name) || (elem name builtinTyNames)
isTyCon _                  = False

isTyConList (TyCon con) | con == GHC.listTyConName = True
isTyConList _                                      = False
                             
isTyVar :: GHC.HsType GHC.Name -> Bool
isTyVar ty@(GHC.HsTyVar _) = not (isTyCon ty)
isTyVar _                  = False

tyBool     = TyCon GHC.boolTyConName
tyInt      = TyCon GHC.intTyConName
tyChar     = TyCon GHC.charTyConName
tyString   = tyList tyChar
tyList τ   = TyApp (TyCon GHC.listTyConName) τ
tyTuple τs = error "TODO: tyTuple"             
             
ptyList (PolyTy ctx ty) = PolyTy ctx $ tyList ty
ptyTuple σs =  PolyTy ctx $ tyTuple τs
    where (ctxs, τs) = unzip $ map (\(PolyTy ctx τ) -> (ctx, τ)) σs
          ctx = concat ctxs
                       
--- Builtin data constructors
-- builtinDataCons = [nilDataCon, consDataCon]
builtinDataCons = [GHC.nilDataCon, GHC.consDataCon, GHC.trueDataCon, GHC.falseDataCon]
builtinDataConNames = map GHC.dataConName builtinDataCons

--- Builtin typeclasses
builtinClassNames = [GHC.numClassName, GHC.fractionalClassName]
                      
--- Types of literals              
typeOfLit :: GHC.HsLit -> Ty
typeOfLit (GHC.HsInt _)    = tyInt
typeOfLit (GHC.HsChar _)   = tyChar
typeOfLit (GHC.HsString _) = tyString

tvsOf :: Ty -> Set.Set Tv
tvsOf (TyCon con)   = mempty
tvsOf (TyVar α)     = Set.singleton α
tvsOf (TyFun τ1 τ2) = tvsOf τ1 `mappend` tvsOf τ2
tvsOf (TyApp τ1 τ2) = tvsOf τ1 `mappend` tvsOf τ2
                        
--- Occurs checking
occurs x (TyVar α)     = α == x
occurs x (TyFun τ1 τ2) = occurs x τ1 || occurs x τ2
occurs x (TyApp τ1 τ2) = occurs x τ1 || occurs x τ2
occurs x _             = False

tyCon :: Ty -> Maybe TyCon
tyCon (TyFun _ _) = Just $ TyConFun
tyCon (TyApp τ _) = tyCon τ
tyCon (TyCon con) = Just $ TyConData con
tyCon _ = Nothing                     
