{-# LANGUAGE FlexibleInstances #-}

module Tandoori.Ty.ShowTy(showTy, showCTy) where

import Tandoori.GHC.Internals
import Tandoori.Ty
import Tandoori.Ty.Canonize
    
data ShowTyCtxt = C { isLeftOfFun :: Bool }                 
             
joinWith sep []     = []
joinWith sep [x]    = x
joinWith sep (x:xs) = (x ++ sep) ++ (joinWith sep xs)
             
showFunLeft :: ShowTyCtxt -> HsType Name -> String
showFunLeft c ty = showTy' c{isLeftOfFun = True} ty
showFunRight c ty = showTy' c{isLeftOfFun = False} ty

showInParen c ty = showTy' c{isLeftOfFun = False} ty                    

forceParen = False

parenIf :: Bool -> String -> String             
parenIf True  s = "(" ++ s ++ ")"
parenIf False s = if forceParen then (parenIf True s) else s
                    
showTy :: HsType Name -> String
showTy ty = showTy' C{isLeftOfFun = False} ty

showCTy :: CanonizedType -> String
showCTy = showTy . uncanonize
            
todo xs = error $ unwords ("showTy: TODO:":xs)                 
unsupported xs = error $ unwords ("showTy: Unsupported:":xs)

showTy' :: ShowTyCtxt -> HsType Name -> String
showTy' c (HsForAllTy e _ lctxt lty)  = (showPreds $ map unLoc $ unLoc lctxt) ++  (showTy $ unLoc lty)
showTy' c (HsTyVar name)              = showName name
showTy' c (HsBangTy HsNoBang lty)     = showTy' c $ unLoc lty
showTy' c (HsBangTy HsStrict lty)     = '!':(showTy' c $ unLoc lty)
showTy' c (HsBangTy HsUnbox lty)      = '!':(showTy' c $ unLoc lty) -- TODO
showTy' c (HsAppTy lty lty')          = unwords [showTy' c $ unLoc lty, showTy' c $ unLoc lty']
showTy' c (HsFunTy lty lty')          = parenIf (isLeftOfFun c) $ unwords [showFunLeft c $ unLoc lty, "->", showFunRight c $ unLoc lty']
showTy' c (HsListTy lty)              = "[" ++ (showTy' c $ unLoc lty) ++ "]"
showTy' c (HsTupleTy boxity ltys)     = "(" ++ (joinWith ", " $ map (showInParen c . unLoc) ltys) ++ ")"
showTy' c (HsOpTy lleft lop lright)   = todo ["HsOpTy"]
showTy' c (HsParTy lty)               = "(" ++ (showInParen c $ unLoc lty) ++ ")"
showTy' c (HsPredTy pred)             = unwords ["HsPredTy", showPred pred] -- unsupported ["HsPredTy", showPred pred]
showTy' c (HsNumTy n)                 = unsupported ["HsNumTy", show n]
showTy' c (HsPArrTy lty)              = unsupported ["HsPArrTy", showTy' c (unLoc lty)]
showTy' c (HsKindSig lty kind)        = unsupported ["HsKindSig"]
showTy' c (HsSpliceTy splice)         = unsupported ["HsSpliceTy"]
showTy' c (HsDocTy lty ldoc)          = unsupported ["HsDocTy", showTy' c (unLoc lty)]
                                               
showName :: Name -> String
showName name = occNameString $ nameOccName name

showPreds :: [HsPred Name] -> String
showPreds [] = ""
showPreds [pred] = unwords [showPred pred, "=> "]
showPreds preds = unwords ["(" ++ (joinWith ", " $ map showPred preds) ++ ")", "=> "]
             
showPred :: HsPred Name -> String
showPred (HsClassP name [lty]) = unwords [showName name, showTy $ unLoc lty]
                
instance Show (HsType Name) where
    show = showTy
           
instance Show CanonizedType where
    show = showCTy 
