module Tandoori.Test where    
    
import IO
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Infix

filename = "input/input-1.hs"
exprname = HsIdent "foo"

defFromBind name decls =
    fromBind $ head  $ filter (\ decl -> case decl of
                                           HsPatBind _ (HsPVar name')  _ _ | name' == name -> True
                                           _ -> False) decls
        where fromBind (HsPatBind _ (HsPVar _) (HsUnGuardedRhs expr) _) = expr
       
getTestExpr :: IO HsExp
getTestExpr = do hSrc <- openFile filename ReadMode
                 contents <- hGetContents hSrc
                 let (ParseOk (HsModule _ _ _ _ decls)) = parseModuleWithMode (ParseMode filename) contents
                 return (defFromBind exprname decls)
