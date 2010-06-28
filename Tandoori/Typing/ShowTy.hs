{-# LANGUAGE FlexibleInstances #-}

module Tandoori.Typing.ShowTy() where

import Tandoori.GHC.Internals
import Tandoori.Typing
    
import Data.List    
    
instance (Show Name) where
    show = showNameShort

showNameShort qname = show $ occNameString $ nameOccName qname           
           
showNameQual qname = show $ modulename ++ "." ++ name ++ "#" ++ uname
    where name = occNameString $ nameOccName qname
          modulename = case nameModule_maybe qname of
                         Nothing -> "?"
                         Just m  -> moduleNameString $ moduleName m
          uname = show $ nameUnique qname

data ShowTyCtxt = C { isLeftOfFun :: Bool, isRightOfApp :: Bool }

showFunLeft :: ShowTyCtxt -> Ty -> String
showFunLeft c ty = showTy' c{isLeftOfFun = True} ty
showFunRight c ty = showTy' c{isLeftOfFun = False} ty

showAppLeft c ty = showTy' c{isRightOfApp = False} ty
showAppRight c ty = showTy' c{isRightOfApp = True} ty
                    
showInParen c ty = showTy' c{isLeftOfFun = False} ty                    

forceParen = False

parenIf :: Bool -> String -> String             
parenIf True  s = "(" ++ s ++ ")"
parenIf False s = if forceParen then (parenIf True s) else s
                    
showTy :: Ty -> String
showTy ty = showTy' C{isLeftOfFun = False, isRightOfApp = False} ty            
            
showTy' :: ShowTyCtxt -> Ty -> String
showTy' c (TyVar α) = showName α
showTy' c (TyCon con) = showName con
showTy' c (TyFun τ1 τ2) = parenIf (isLeftOfFun c) $ unwords [showFunLeft c τ1, "->", showFunRight c τ2]
showTy' c (TyApp τ1 τ2) | isTyConList τ1 = "[" ++ showFunRight c τ2 ++ "]"
                        | otherwise      = parenIf (isRightOfApp c) $ unwords [showAppLeft c τ1, showAppRight c τ2]           
-- showTy' c (HsTupleTy boxity ltys)     = "(" ++ (joinWith ", " $ map (showInParen c . unLoc) ltys) ++ ")"
                                               
showName :: Name -> String
showName name = occNameString $ nameOccName name

showPreds :: OverCtx -> String
showPreds [] = ""
showPreds [pred] = unwords [showPred pred, "=> "]
showPreds preds = unwords ["(" ++ (intercalate ", " $ map showPred preds) ++ ")", "=> "]

showPred :: OverPred -> String
showPred (cls, τ) = unwords [showName cls, show τ]

instance Show Ty where
    show = showTy' C{isLeftOfFun = False, isRightOfApp = False}

instance Show OverTy where
    show (OverTy ctx τ) = showPreds ctx ++ show τ
           
instance Show PolyTy where
    show = show . fromPolyTy
           
instance (Show TyEq) where
    show (τ :=: τ') = unwords [show τ, ":=:", show τ']
