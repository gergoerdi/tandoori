module Tandoori.Typing where

import qualified Tandoori.GHC.Internals as GHC
    
import qualified Data.Set as Set
import Data.List (nub)
import Data.Monoid
import Control.Monad (liftM, liftM2)
    
type Tv = GHC.Name    
type Con = GHC.Name
type Cls = GHC.Name
    
data Ty = TyVar Tv
        | TyCon Con
        | TyApp Ty Ty
        | TyFun Ty Ty
        | TyTuple Int
        -- TODO: records
        deriving (Eq, Ord)

mapTy :: Monad m => (Tv -> m Tv) -> (Ty -> m Ty)
mapTy f (TyVar α) = liftM TyVar $ f α
mapTy f (TyApp τ τ') = liftM2 TyApp (mapTy f τ) (mapTy f τ')
mapTy f (TyFun τ τ') = liftM2 TyFun (mapTy f τ) (mapTy f τ')
mapTy f τ = return τ

data TyCon = TyConData Con
           | TyConFun
           | TyConTuple Int
           deriving (Eq, Ord)
          
data TyEq = Ty :=: Ty
          
type OverPred = (Cls, Ty)
type OverCtx = [OverPred]
data OverTy = OverTy OverCtx Ty

type PolyPred = (Cls, Tv)
type PolyCtx = [PolyPred]
data PolyTy = PolyTy PolyCtx Ty

fromPolyCtx :: PolyCtx -> OverCtx
fromPolyCtx ctx = map (fmap TyVar) ctx

fromPolyTy :: PolyTy -> OverTy
fromPolyTy (PolyTy ctx ty) = OverTy (fromPolyCtx ctx) ty
              
--- Utility
tyCurryApp :: [Ty] -> Ty
tyCurryApp τs = foldl1 TyApp τs                                

tyUncurryApp :: Ty -> [Ty]
tyUncurryApp (TyApp τ1 τ2) = tyUncurryApp τ1 ++ [τ2]
tyUncurryApp τ = [τ]
                
tyCurryFun :: [Ty] -> Ty
tyCurryFun τs = foldr1 TyFun τs

ptyCurryFun :: [PolyTy] -> PolyTy
ptyCurryFun σs = PolyTy ctx' τ'
    where (ctxs, τs) = unzip $ map (\ (PolyTy ctx τ) -> (ctx, τ)) σs
          ctx' = nub $ concat ctxs
          τ' = tyCurryFun τs
                
--- Builtin types                
builtinTyNames = [GHC.boolTyConName, GHC.intTyConName, GHC.charTyConName, GHC.listTyConName]
-- builtinTyNames = [intTyConName, charTyConName, listTyConName]

isTyCon :: GHC.HsType GHC.Name -> Bool                             
isTyCon (GHC.HsTyVar name) = (GHC.isTyConName name) || (elem name builtinTyNames)
isTyCon _                  = False

isTyConList (TyCon con) | con == GHC.listTyConName = True
isTyConList _                                      = False

isTyConTuple τ = isTyConTuple' 1 τ
    where isTyConTuple' n (TyTuple m) = n == m
          isTyConTuple' n (TyApp τ1 τ2) = isTyConTuple' (succ n) τ1
          isTyConTuple' _ _ = False
                                                     
isTyVar :: GHC.HsType GHC.Name -> Bool
isTyVar ty@(GHC.HsTyVar _) = not (isTyCon ty)
isTyVar _                  = False

tyBool     = TyCon GHC.boolTyConName
tyInt      = TyCon GHC.intTyConName
tyChar     = TyCon GHC.charTyConName
tyString   = tyList tyChar
tyList τ   = TyApp (TyCon GHC.listTyConName) τ
tyTuple τs = tyCurryApp $ (TyTuple (length τs)):τs
             
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
tvsOf (TyTuple _)   = mempty
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
tyCon (TyTuple n) = Just $ TyConTuple n
tyCon _ = Nothing                     
