module Tandoori (TanType, TanExpr, TanPat, VarName, ConName, TvName) where

import Tandoori.GHC.Internals

type TanType = HsType Name
type TanExpr = HsExpr Name
type TanPat = Pat Name
type VarName = Name
type ConName = Name
type TvName = Name
