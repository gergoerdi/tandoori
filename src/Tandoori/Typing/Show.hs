module Tandoori.Typing.Show(printCtxt, showName) where

import Tandoori.GHC.Internals
import Tandoori
import Tandoori.Typing
import Tandoori.Typing.Pretty
import Tandoori.Typing.Error
import Tandoori.Typing.Ctxt
import Tandoori.Typing.MonoEnv

import Control.Applicative ((<$>))
import qualified Data.Map as Map    
import qualified Data.Set as Set
import qualified Text.PrettyPrint.Boxes as Box
import Data.List    
-- import qualified Data.MultiSet as Bag

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
showName name = if isSymOcc occName || isDataSymOcc occName then "(" ++ s ++ ")" else s
  where occName = nameOccName name
        s = occNameString occName

showPolyPred :: PolyPred -> String
showPolyPred (cls, α) = unwords [showName cls, showName α]

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

instance Outputable ErrorMessage where
    ppr (ErrorMessage (ErrorLocation srcloc Nothing) content)     = ppr srcloc <> colon <+> ppr content
    ppr (ErrorMessage (ErrorLocation srcloc (Just src)) content)  = ppr srcloc <> colon $$ src $$ ppr content
                                  
showFailedEqs sep tyeqs = unwords $ map (\ (t1 :=: t2) -> unwords [show t1, sep, show t2]) tyeqs

instance Outputable TypingErrorContent where
    ppr (Unsolvable (τ1 :=: τ2)) = text "Cannot unify" <+> quotes (text (show τ1)) <+> text "with" <+> quotes (text (show τ2))
    ppr (InfiniteType (τ1 :=: τ2)) = text "Occurs check failed: infinite type" <+> text (show τ1) <+> text "=" <+> text (show τ2)
    ppr (UnfulfilledPredicate pred) = text "Unfulfilled predicate" <+> text (showPred pred)
                          
instance Outputable ErrorContent where
    ppr (UndefinedCon name) = text "Reference to undefined constructor" <+> quotes (ppr name)    
    ppr (UndefinedVar name) = text "Reference to undefined variable" <+> quotes (ppr name)      
    ppr (UnificationFailed ms tyerr) = 
      ppr tyerr' <+> source <> colon $$ text (Box.render $ boxMonos ms')
        where (ms', tyerr') = runPretty $ do ms' <- mapM prettyMonoM ms
                                             tyerr' <- prettyTypingErrorM (typingErrorContent tyerr)
                                             return (ms', tyerr')
              source = case typingErrorSrc tyerr of
                Just x -> text "when unifying " <> (quotes . text . showName) x
                Nothing -> empty                
    ppr (CantFitDecl σDecl (m, τ)) = 
      text "Declared type" <+> text (show σDecl) <+> 
      text "is not a special case of inferred typing" <+> text (showTyping m τ)      
    ppr (InvalidCon σ) = 
      text "Invalid constructor signature" <+> text (show σ)      
    ppr (ClassCycle clss) = 
      text "Cycle in superclass hierarchy" <> colon <+> 
      sep (punctuate comma $ map (quotes . text . showName) clss)
    ppr (AmbiguousPredicate j (cls, α))  = 
      text "Ambiguous predicate" <+> text (showPred (cls, τ'))
        where τ' = prettyTy (TyVar α)
    ppr (MissingBaseInstances (cls, τ) πs) = 
      text "Missing base instances of" <+> text (showPred (cls, τ)) <> colon <+> 
      sep (punctuate comma $ map (text . showPred) $ fromPolyCtx πs)
    ppr InvalidInstance =  text "Invalid instance declaration"
    ppr (UndefinedCls cls) = text "Undefined class" <+> text (showName cls)
    ppr (OtherError message) = text message
    
prettyTypingErrorM (Unsolvable eq) = Unsolvable <$> prettyTyEqM eq
prettyTypingErrorM (InfiniteType eq) = InfiniteType <$> prettyTyEqM eq
prettyTypingErrorM (UnfulfilledPredicate (cls, τ)) = do τ' <- prettyTyM τ
                                                        return $ UnfulfilledPredicate (cls, τ')
                                       
prettyTyEqM (t :=: u) = do t' <- prettyTyM t
                           u' <- prettyTyM u
                           return $ t' :=: u'
                      
prettyMonoM = mapMonoM prettyTvM

instance Show MonoEnv where
  show m = "{" ++ intercalate ", " (typing ++ preds) ++ "}"
    where typing = map (\ (x, τ) -> showName x ++ "::" ++ show τ) $ getMonoVars m
          preds = map (\ (cls, α) -> unwords [showName cls, showName α]) $ getMonoPreds m

showTyping m τ = runPretty $ do 
  m' <- prettyMonoM m  
  τ' <- prettyTyM τ
  return $ unwords [show m', "⊢", show τ']
  
printCtxt :: Ctxt -> IO ()
printCtxt c = Box.printBox $ boxName Box.<+> boxType
    where showPolyTy = show . runPretty . prettyPolyTyM
          pairs = (map (\ (name, (L _ σ)) -> (showName name, show σ)) $ Map.toList $ userDecls c) ++
                  (map (\ (name, (m, τ)) -> (showName name,  showTyping m τ)) $ Map.toList $ polyVars c)     
          boxName = Box.vcat Box.left $ map (Box.text . fst) pairs
          boxType = Box.vcat Box.left $ map (\ (name, typing) -> Box.text "::" Box.<+> Box.text typing) pairs

boxMonos :: [MonoEnv] -> Box.Box
boxMonos ms = Box.hsep 2 Box.top $ boxNames:(map boxTypes ms)
    where vars :: [VarName]
          vars = Set.toList $ Set.unions $ map (Set.fromList . map fst . getMonoVars) ms
          -- Omit non-shared variables?
          -- vars = map fst $ filter (\(v, c) -> c >= 2) $ Bag.toOccurList $ Bag.unions varBags
          --   where varBags = map (Bag.fromList . nub . map fst . getMonoVars) ms
          
          boxType m v = Box.text $ maybe "" show $ getMonoVar m v
                          
          boxNames = Box.vcat Box.left $ 
                     (Box.text ""):
                     (Box.text ""):
                     (Box.text "Predicates:"):
                     (map ((Box.<+> Box.text "::").(Box.text . showName)) vars)
          boxTypes m = Box.vcat Box.left $ boxSrc:boxTy:boxPreds:boxsTyVars
            where 
              boxSrc = Box.text $ maybe "" showSDocUnqual $ getMonoSrc m
              boxTy = Box.text $ maybe "" show $ getMonoTy m
              boxsTyVars = map (boxType m) vars
              boxPreds = Box.text $ intercalate ", " $ map showPolyPred (getMonoPreds m)
                  
