module Tandoori.Util (genLoc, noBinder) where

import Tandoori.GHC.Internals

genLoc x = mkGeneralLocated "(internal)" x

noBinder = error "HsForAllTy binder access is unsupported" 
