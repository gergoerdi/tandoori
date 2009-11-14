module Tandoori (TanType, TanExpr, TanPat, VarName, ConName, TvName) where

import Name
import HsTypes (HsType)
import HsPat (Pat)
import HsExpr (HsExpr)
    
type TanType = HsType Name
type TanExpr = HsExpr Name
type TanPat = Pat Name
type VarName = Name
type ConName = Name
type TvName = Name
