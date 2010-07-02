{-# LANGUAGE FlexibleInstances #-}
module Tandoori.Typing.Show(printCtxt, showName) where

import Tandoori.GHC.Internals
import Tandoori.Typing
import Tandoori.Typing.Pretty
import Tandoori.Typing.Error
import Tandoori.Typing.Ctxt

import Control.Applicative    
import qualified Data.Map as Map    
import Data.List    
    
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
showTy' c τ@(TyApp τ1 τ2) | isTyConList τ1 = "[" ++ showFunRight c τ2 ++ "]"
                          | isTyConTuple τ1 = let τs = tail $ tyUncurryApp τ
                                              in "(" ++ commaList (map show τs) ++ ")"
                          | otherwise      = parenIf (isRightOfApp c) $ unwords [showAppLeft c τ1, showAppRight c τ2]
showTy' c (TyTuple n) = "(" ++ replicate (pred n) ',' ++ ")"
                                               
showName :: Name -> String
showName name = occNameString $ nameOccName name

showPreds :: OverCtx -> String
showPreds [] = ""
showPreds [pred] = unwords [showPred pred, "=> "]
showPreds preds = unwords ["(" ++ (commaList $ map showPred preds) ++ ")", "=> "]

commaList = intercalate ", "                  
                  
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

instance Outputable TypingError where
    ppr (Unsolvable (τ1 :=: τ2)) = text "Cannot unify" <+> text (show τ1) <+> text "with" <+> text (show τ2)
    ppr (InfiniteType (τ1 :=: τ2)) = text "Occurs check failed: infinite type" <+> text (show τ1) <+> text "=" <+> text (show τ2)
                          
instance Outputable ErrorContent where
    ppr (UndefinedCon name)              = text "Reference to undefined constructor" <+> quotes (ppr name)
    ppr (UndefinedVar name)              = text "Reference to undefined variable" <+> quotes (ppr name)
    ppr (UnificationFailed ms tyerr)     = ppr tyerr'
        where tyerr' = runPretty $ prettyTypingErrorM tyerr
    ppr (CantFitDecl tyDecl ty)          = text "Declared type" <+> text (show tyDecl') <+> text "is not a special case of inferred type" <+> text (show ty')
        where (tyDecl', ty') = runPretty $ do σDecl' <- prettyPolyTyM tyDecl
                                              σ' <- prettyPolyTyM ty
                                              return (σDecl', σ')
    ppr (InvalidCon σ)                   = text "Invalid constructor signature" <+> text (show σ)
    ppr (ClassCycle clss)                = text "Cycle in superclass hierarchy" <> colon <+> sep (punctuate comma $ map (quotes . text . showName) clss)
    ppr (AmbiguousPredicate σ (cls, α))  = text "Ambiguous predicate" <+> text (showPred (cls, τ')) <+> text "for type" <+> text (show σ')
        where (σ', τ') = runPretty $ do σ' <- prettyPolyTyM σ
                                        τ' <- prettyTyM (TyVar α)
                                        return (σ', τ')
    ppr (UnfulfilledPredicate (cls, τ))  = text "Unfulfilled predicate" <+> text (showPred (cls, τ'))
        where τ' = prettyTy τ
    ppr (MissingBaseInstances (cls, τ) πs) = text "Missing base instances of" <+> text (showPred (cls, τ)) <> colon <+> sep (punctuate comma $ map (text . showPred) $ fromPolyCtx πs)
    ppr InvalidInstance                  = text "Invalid instance declaration"
    ppr (OtherError message  )           = text message
                                           
prettyTypingErrorM (Unsolvable eq) = Unsolvable <$> prettyTyEqM eq
prettyTypingErrorM (InfiniteType eq) = InfiniteType <$> prettyTyEqM eq
                                           
prettyTyEqM (t :=: u) = do t' <- prettyTyM t
                           u' <- prettyTyM u
                           return $ t' :=: u'
                      

printCtxt :: Ctxt -> IO ()
printCtxt c = mapM_  print $ ((map (\ (name, (m, σ)) -> (showName name, σ)) $ Map.toList $ polyVars c) ++
                              (map (\ (name, (L _ σ)) -> (showName name, σ)) $ Map.toList $ userDecls c))
    
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

                                  
