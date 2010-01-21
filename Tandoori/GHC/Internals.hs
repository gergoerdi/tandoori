module Tandoori.GHC.Internals
    (module SrcLoc,
     module Outputable,
     module Name,
     module BasicTypes,
     module Unique,
     module FastString,
     module HsExpr,
     module HsTypes,
     module HsPat,
     module HsLit,
     module HsBinds,        
     module DataCon,
     module TysWiredIn,
     module PrelNames,
     module HsDecls) where

import SrcLoc
import Outputable
import Name
import BasicTypes
import Unique
import FastString
import HsExpr
import HsTypes
import HsPat
import HsLit
import HsBinds
import DataCon (dataConName)
import TysWiredIn (intTyConName, charTyConName, boolTyConName, listTyConName, nilDataCon, consDataCon)
import PrelNames (stringTyConName, eqClassName, ordClassName, numClassName, fractionalClassName)
import HsTypes
import HsDecls
