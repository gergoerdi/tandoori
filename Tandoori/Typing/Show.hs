{-# LANGUAGE FlexibleInstances #-}
module Tandoori.Typing.Show(printCtxt) where

import Tandoori.GHC.Internals
import Tandoori.Typing
import Tandoori.Typing.Pretty
import Tandoori.Typing.Monad
import Tandoori.Typing.Ctxt

import qualified Data.Map as Map    
import Data.List    
    
instance Show Name where
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

instance Outputable ErrorSource where
    ppr (ErrorSource src) = ppr src
                  
instance Outputable ErrorMessage where
    ppr (ErrorMessage (ErrorLocation srcloc Nothing) content)     = ppr srcloc <> colon <+> ppr content
    ppr (ErrorMessage (ErrorLocation srcloc (Just src)) content)  = ppr srcloc <> colon $$ ppr src $$ ppr content
                                  
showFailedEqs sep tyeqs = unwords $ map (\ (t1 :=: t2) -> unwords [show t1, sep, show t2]) tyeqs

instance Outputable ErrorContent where
    ppr (UndefinedCon name)              = text "Reference to undefined constructor" <+> quotes (ppr name)
    ppr (UndefinedVar name)              = text "Reference to undefined variable" <+> quotes (ppr name)
    ppr (UnificationFailed ms tyeqs)     = text "Cannot unify" <+> text (showFailedEqs "with" tyeqs')
        where tyeqs' = runPretty $ mapM prettyTyEqM tyeqs
    ppr (CantFitDecl tyDecl ty tyeqs)    = text "Declared type" <+> text (show tyDecl') <+> text "is not a special case of inferred type" <+> text (show ty')
        where (tyDecl', ty') = runPretty $ do σDecl' <- prettyPolyTyM tyDecl
                                              σ' <- prettyPolyTyM ty
                                              return (σDecl', σ')
    ppr (InvalidCon σ)                   = text "Invalid constructor signature" <+> text (show σ)
    ppr (ClassCycle clss)                = text "Cycle in superclass hierarchy:" <+> sep (punctuate comma (map (quotes . text . showName) clss))
    ppr (AmbiguousPredicate σ (cls, α))  = text "Ambiguous predicate" <+> text (showPred (cls, τ')) <+> text "for type" <+> text (show σ')
        where (σ', τ') = runPretty $ do σ' <- prettyPolyTyM σ
                                        τ' <- prettyTyM (TyVar α)
                                        return (σ', τ')
    ppr (UnfulfilledPredicate (cls, τ))  = text "Unfulfilled predicate" <+> text (showPred (cls, τ'))
        where τ' = prettyTy τ
    ppr (OtherError message  )           = text message

prettyTyEqM (t :=: u) = do t' <- prettyTyM t
                           u' <- prettyTyM u
                           return $ t' :=: u'
                      

printCtxt :: Ctxt -> IO ()
printCtxt c = mapM_  print $ ((map (\ (name, (m, σ)) -> (name, σ)) $ Map.toList $ polyVars c) ++
                              (map (\ (name, (L _ σ)) -> (name, σ)) $ Map.toList $ userDecls c))
    
-- printCtxt :: Ctxt -> IO ()             
-- printCtxt c = do print $ tabTy (rowsDecl ++ rowsInfer)
                          
--     where showNameShort qname = occNameString $ nameOccName qname
--           showTy ty = show $ prettyTy ty
--           showCTy cty = show $ prettyTy $ uncanonize cty
--           --showTy ty = showSDocUnqual $ ppr $ prettyTy ty
                                
--           rowFromInfer name (m, cty) = (showNameShort name, showCTy cty)
--           rowFromDecl name cty = (showNameShort name, showCTy cty)

--           rowTy (sname, sty) = [sname, "::", sty]
--           tabTy rows = fromRows $ map rowTy rows
                                
--           rowsInfer = map (uncurry rowFromInfer) $ Map.toList $ polyVars c
--           rowsDecl = map (uncurry rowFromDecl) $ map (\ (name, lty) ->  (name, unLoc lty)) $ Map.toList $ userDecls c

                                  
