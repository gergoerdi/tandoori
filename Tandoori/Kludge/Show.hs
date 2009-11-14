{-# LANGUAGE StandaloneDeriving, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, UndecidableInstances #-}

module Tandoori.Kludge.Show where

import RdrName
import OccName
import Name
import GHC
import BasicTypes    
import Bag   
import TypeRep
import UniqFM
import ForeignCall
import SrcLoc
import Outputable

import Tandoori.Ty.PolyEnv
import Tandoori.Ty.MonoEnv

    
instance Show a => Show (Located a) where
    show lx = "(L " ++ show (unLoc lx) ++ ")"
    
-- instance Show id => (Show (HsDecl id)) where
--     show (TyClD tyClDecl) = "TyClD"
--     show (InstD _) = "InstD"
--     show (ValD bind) = "ValD: '" ++ (show bind) ++ "'"

deriving instance Show (HsDecl Name)
deriving instance Show (HsBind Name)
deriving instance Show (HsBind Id)
deriving instance Show (DocDecl Name)
deriving instance Show (DocDecl Id)
deriving instance Show (SpliceDecl Name)
deriving instance Show (SpliceDecl Id)
deriving instance Show (RuleDecl Name)
deriving instance Show (RuleDecl Id)
deriving instance Show (WarnDecl Name)
deriving instance Show (WarnDecl Id)
deriving instance Show (Pat Name)
deriving instance Show (Pat Id)
deriving instance Show (GRHSs Name)
deriving instance Show (GRHSs Id)
deriving instance Show (GRHS Name)
deriving instance Show (GRHS Id)
deriving instance Show (HsExpr Name)
deriving instance Show (HsExpr Id)
deriving instance Show WarningTxt
deriving instance Show (IPName Name)
deriving instance Show (IPName Id)
deriving instance Show (Stmt Name)
deriving instance Show (Stmt Id)
deriving instance Show (HsLocalBinds Name)
deriving instance Show (HsLocalBinds Id)
--deriving instance Show (HsType Name)
deriving instance Show (HsType Id)
deriving instance Show (HsStmtContext Name)
deriving instance Show (HsStmtContext Id)
deriving instance Show (GroupByClause Name)
deriving instance Show (GroupByClause Id)
deriving instance Show (HsIPBinds Name)
deriving instance Show (HsIPBinds Id)
deriving instance Show (IPBind Name)
deriving instance Show (IPBind Id)
deriving instance Show (ArithSeqInfo Name)
deriving instance Show (ArithSeqInfo Id)
deriving instance Show (HsBracket Name)
deriving instance Show (HsBracket Id)
deriving instance Show (HsSplice Name)
deriving instance Show (HsSplice Id)
deriving instance Show (HsGroup Name)
deriving instance Show (HsGroup Id)
deriving instance Show (ForeignDecl Name)
deriving instance Show (ForeignDecl Id)
deriving instance Show (DefaultDecl Name)
deriving instance Show (DefaultDecl Id)
deriving instance Show (FixitySig Name)
deriving instance Show (FixitySig Id)
deriving instance Show (DerivDecl Name)
deriving instance Show (DerivDecl Id)
deriving instance Show (InstDecl Name)
deriving instance Show (InstDecl Id)
deriving instance Show (TyClDecl Name)
deriving instance Show (TyClDecl Id)
deriving instance Show (ConDecl Name)
deriving instance Show (ConDecl Id)
deriving instance Show (HsPred Name)
deriving instance Show (HsPred Id)
deriving instance Show (HsTyVarBndr Name)
deriving instance Show (HsTyVarBndr Id)
deriving instance Show (Match Name)
deriving instance Show (Match Id)
deriving instance (Show a, Show b) => Show (HsConDetails a b)
deriving instance (Show a, Show b) => Show (HsRecFields a b)
deriving instance (Show a, Show b) => Show (HsRecField a b)
deriving instance Show NewOrData
deriving instance Show FamilyFlavour
deriving instance Show FoType
deriving instance Show HsWrapper
deriving instance Show Prag
deriving instance Show OverLitVal
deriving instance Show HsLit
deriving instance Show Boxity
deriving instance Show InlineSpec        
deriving instance Show Activation         
deriving instance Show Type
deriving instance Show PredType         
deriving instance Show ForeignImport
deriving instance Show ForeignExport
deriving instance Show CExportSpec
deriving instance Show DNCallSpec
deriving instance Show CImportSpec
deriving instance Show CCallConv
deriving instance Show CCallTarget
deriving instance Show DNType
deriving instance Show DNKind
deriving instance Show HsExplicitForAll
deriving instance Show HsBang
deriving instance Show Fixity
deriving instance Show FixityDirection
deriving instance Show HsArrAppType
deriving instance Show a => Show (UniqFM a)
deriving instance Show (RuleBndr Name)
deriving instance Show (RuleBndr Id)
deriving instance Show (ResType Name)
deriving instance Show (ResType Id)
deriving instance Show (ConDeclField Name)
deriving instance Show (ConDeclField Id)
deriving instance Show (HsQuasiQuote Name)
deriving instance Show (HsQuasiQuote Id)
deriving instance Show (HsCmdTop Name)
deriving instance Show (HsCmdTop Id)
deriving instance Show (HsMatchContext Name)
deriving instance Show (HsMatchContext Id)
deriving instance Show (Sig Name)
deriving instance Show (Sig Id)
deriving instance Show (HsModule Name)
deriving instance Show (HaddockModInfo Name)
deriving instance Show (ImportDecl Name)
deriving instance Show (IE Name)

deriving instance Show PolyEnv
deriving instance Show MonoEnv         

instance (Show (HsType Name)) where
    -- show (HsTyVar x) = showNameShort x
    -- show (HsFunTy ty ty') = "(" ++ unwords [(show ty), "->", (show  ty')] ++ ")"
    -- show (HsTupleTy boxity tys) = joinWith ", " $ map show tys
    --     where joinWith sep [] = []
    --           joinWith sep [x] = x
    --           joinWith sep (x:xs) = x ++ sep ++ (joinWith sep xs)
    show ty = showSDoc $ ppr ty
         
instance (Show TyCon) where
    show _ = "TyCon"

instance (Show (Match id)) => (Show (MatchGroup id)) where
    show (MatchGroup lms ptt) = unwords ["MatchGroup", show lms, "PostTcType"]
             
instance (Show SrcSpan) where
    show _ = "SrcSpan"
         
instance (Show OccName) where
     show = show . showSDocDump . ppr

instance (Show RdrName) where
    show (Unqual name) = unwords ["Unqual", show name]
    show (Qual modname name) = unwords ["Qual", show name]
    show (Orig modname name) = unwords ["Orig", show name]
    show (Exact name) = unwords ["Exact", show name]

instance Show (HsOverLit id) where
    show (OverLit{ol_val=val}) = unwords ["OverLit", "{ol_val", "=", (show val) ++ "}"]

showList' :: [String] -> String
showList' []     = "[]"
showList' (s:ss) = "[" ++ s ++ (concat $ map (',':) ss) ++ "]"

instance (Show a) => Show (Bag a) where
    show b = unwords ["Bag", show $ bagToList b]
                   
instance (Show (HsBind id)) => (Show (HsValBinds id)) where
    show (ValBindsIn binds sigs) = "ValBindsIn"
    show (ValBindsOut reclbinds lsigs) = unwords ["ValBindsOut", (showList' $ map showRecBinds reclbinds), (show lsigs)]
        where showRecBinds (Recursive, lbinds) = "(Recursive," ++ showBinds lbinds ++ ")"
              showRecBinds (NonRecursive, lbinds) = "(NonRecrusive," ++ showBinds lbinds ++ ")"
              showBinds lbinds = showList' $ map show $ bagToList lbinds

instance (Show ModuleName) where
    show = moduleNameString
                                 
instance (Show Name) where
    show = showNameShort

showNameShort qname = show $ occNameString $ nameOccName qname           
           
showNameQual qname = show $ modulename ++ "." ++ name ++ "#" ++ uname
    where name = occNameString $ nameOccName qname
          modulename = case nameModule_maybe qname of
                         Nothing -> "?"
                         Just m  -> moduleNameString $ moduleName m
          uname = show $ nameUnique qname
