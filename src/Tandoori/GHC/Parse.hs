module Tandoori.GHC.Parse (parseMod, getDecls) where    

import Tandoori.GHC.Internals    
    
import HsSyn (hsmodDecls)
import Parser (parseModule)
import Lexer (unP, mkPState, ParseResult(..))
import StringBuffer (hGetStringBuffer)
import DynFlags (defaultDynFlags)
    
getDecls mod = hsmodDecls $ unLoc mod

parseMod src_filename = do buf <- hGetStringBuffer src_filename
                           let loc = mkSrcLoc (mkFastString src_filename) 1 0
                               dflags = defaultDynFlags
                           case unP Parser.parseModule (mkPState dflags buf loc) of
                             POk pst rdr_module -> return rdr_module
                             PFailed srcspan message -> error $ showSDoc message
