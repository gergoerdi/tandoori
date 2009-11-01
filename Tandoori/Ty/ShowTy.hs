{-# LANGUAGE FlexibleInstances #-}

module Tandoori.Ty.ShowTy(showTy) where

import Tandoori.GHC.Internals

data Context = C { isLeftOfFun :: Bool }                 
             
joinWith sep []     = []
joinWith sep [x]    = x
joinWith sep (x:xs) = (x ++ sep) ++ (joinWith sep xs)
             
showFunLeft :: Context -> HsType Name -> String
showFunLeft c ty = showTy' c{isLeftOfFun = True} ty
showFunRight c ty = showTy' c{isLeftOfFun = False} ty

showInParen c ty = showTy' c{isLeftOfFun = False} ty                    
                    
parenIf True  s = "(" ++ s ++ ")"
parenIf False s = s
                    
showTy :: HsType Name -> String
showTy ty = showTy' C{isLeftOfFun = False} ty

unsupported xs = error $ unwords ("showTy: TODO:":xs)
                
showTy' :: Context -> HsType Name -> String
showTy' c (HsForAllTy _ bndrs lctxt lty) = showTy $ unLoc lty
showTy' c (HsTyVar name)                 = showName name
showTy' c (HsBangTy HsNoBang lty)        = showTy' c $ unLoc lty
showTy' c (HsBangTy _ lty)               = '!':(showTy' c $ unLoc lty)
showTy' c (HsAppTy lty lty')             = unwords [showTy' c $ unLoc lty, showTy' c $ unLoc lty']
showTy' c (HsFunTy lty lty')             = parenIf (isLeftOfFun c) $ unwords [showFunLeft c $ unLoc lty, "->", showFunRight c $ unLoc lty']
showTy' c (HsListTy lty)                 = "[" ++ (showTy' c $ unLoc lty) ++ "]"
showTy' c (HsPArrTy lty)                 = unsupported ["HsPArrTy"]
showTy' c (HsTupleTy boxity ltys)        = "(" ++ (joinWith ", " $ map (showInParen c . unLoc) ltys) ++ ")"
showTy' c (HsOpTy lleft lop lright)      = unsupported ["HsOpTy"]
showTy' c (HsParTy lty)                  = "(" ++ (showInParen c $ unLoc lty) ++ ")"
showTy' c (HsNumTy n)                    = unsupported ["HsNumTy", show n]
showTy' c (HsPredTy pred)                = unsupported ["HsPredTy"]
showTy' c (HsKindSig lty kind)           = unsupported ["HsKindSig"]
showTy' c (HsSpliceTy splice)            = unsupported ["HsSpliceTy"]
showTy' c (HsDocTy lty ldoc)             = unsupported ["HsDocTy"]
                                               
showName :: Name -> String
showName name = occNameString $ nameOccName name

instance Show (HsType Name) where
    show = showTy 
