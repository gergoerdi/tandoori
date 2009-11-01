module Tandoori.GHC.Parse (parseMod, getDecls) where

import HsSyn   
import Parser
import Lexer
import StringBuffer
import SrcLoc
import FastString    
import DynFlags ( defaultDynFlags )    
    
getDecls mod = hsmodDecls $ unLoc mod

parseMod src_filename = do buf <- hGetStringBuffer src_filename
                           let loc = mkSrcLoc (mkFastString src_filename) 1 0
                               dflags = defaultDynFlags
                           case unP Parser.parseModule (mkPState buf loc dflags) of
                             POk pst rdr_module -> return rdr_module