module Tandoori (VarName, ConName, TvName, infixToPrefix) where

import Language.Haskell.Syntax    
    
type VarName = HsName
type ConName = HsName
type TvName = HsName
    
infixToPrefix (HsInfixApp left (HsQVarOp opname) right) = HsApp (HsApp (HsVar opname) left) right
infixToPrefix (HsInfixApp left (HsQConOp conname) right) = HsApp (HsApp (HsCon conname) left) right
