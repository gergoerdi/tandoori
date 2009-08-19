module Tandoori.Scope (boundNamesOfPatInt, boundNamesOfDeclInt, boundNamesOfDeclExt) where

import Tandoori
import Language.Haskell.Syntax

data Depth = Top | Deep           

concatunzip :: [([a], [b])] -> ([a], [b])
concatunzip tups = (fsts, snds)
    where fsts = concat $ map fst tups
          snds = concat $ map snd tups
           
boundNamesOfPat :: Depth -> HsPat -> ([VarName], [VarName])
boundNamesOfPat Top  (HsPVar name)       = ([name], [])
boundNamesOfPat Deep (HsPVar name)       = ([], [name])
boundNamesOfPat d    (HsPNeg pat)        = boundNamesOfPat d pat
boundNamesOfPat d    (HsPApp con pats)   = concatunzip $ map (boundNamesOfPat Deep) pats
boundNamesOfPat d    (HsPInfixApp l o r) = concatunzip [boundNamesOfPat d l, boundNamesOfPat d r]                                           
boundNamesOfPat d    (HsPList pats)      = concatunzip $ map (boundNamesOfPat d) pats
boundNamesOfPat d    (HsPTuple pats)     = concatunzip $ map (boundNamesOfPat d) pats                                           
boundNamesOfPat d    (HsPAsPat name pat) = let (exts, ints) = boundNamesOfPat d pat
                                           in case d of Top -> (name:exts, ints)
                                                        Deep -> (exts, name:ints)
boundNamesOfPat d    (HsPIrrPat pat)     = boundNamesOfPat d pat
boundNamesOfPat d    (HsPParen pat)      = boundNamesOfPat d pat
boundNamesOfPat d    (HsPLit _)          = ([], [])
boundNamesOfPat d    HsPWildCard         = ([], [])

boundNamesOfPatInt :: HsPat -> [VarName]
boundNamesOfPatInt pat = exts ++ ints
    where (exts, ints) = boundNamesOfPat Top pat
                                           
boundNamesOfDecl :: HsDecl -> ([VarName], [VarName])
boundNamesOfDecl (HsPatBind _ pat _ _)                     = boundNamesOfPat Top pat
boundNamesOfDecl (HsFunBind ms@((HsMatch _ name _ _ _):_)) = let (exts, ints) = concatunzip $ map boundNamesOfMatch ms
                                                             in (name:exts, ints)
boundNamesOfDecl decl                                      = error $ "boundNamesOfDecl: Unsupported decl " ++ (show decl)

boundNamesOfDeclInt :: HsDecl -> [VarName]
boundNamesOfDeclInt decl = exts ++ ints
    where (exts, ints) = boundNamesOfDecl decl

boundNamesOfDeclExt :: HsDecl -> [VarName]
boundNamesOfDeclExt decl = exts
    where (exts, ints) = boundNamesOfDecl decl
                                             
boundNamesOfMatch :: HsMatch -> ([VarName], [VarName])
boundNamesOfMatch (HsMatch _ _ pats _ _) = concatunzip $ map (boundNamesOfPat Deep) pats
