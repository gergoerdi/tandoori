module Tandoori.Ty.Pretty(prettyTy, prettyTyM, runPretty) where

import Tandoori.GHC.Internals
import Tandoori
    
import Data.Char
import Control.Monad.State    
import qualified Data.Map as Map

type NameMap = Map.Map Name Name    
data PrettyS = PrettyS { count :: Int,
                         namemap :: NameMap }

mkPrettyS = PrettyS { count = 0, namemap = Map.empty }
                 
isPrettyName :: Name -> Bool
isPrettyName = not . isSystemName

prettyNameM :: Name -> State PrettyS Name
prettyNameM name | isPrettyName name = return name
                   | otherwise         = do s@PrettyS{namemap = map, count = c} <- get
                                            case Map.lookup name map of
                                              Just name' -> return name'
                                              Nothing    -> do let name' = prettyName name c
                                                                   s' = s{namemap = Map.insert name name' map, count = c + 1}
                                                               put s'
                                                               return name'
    where prettyName name c = localiseName name'
              where name' = mkSysTvName u (mkFastString namestr)
                    u = nameUnique name
                    namestr | c < 26    = [chr $ (ord 'a') + c]
                            | otherwise = 't':(show (c - 26))

prettyLTyM :: LHsType Name -> State PrettyS (LHsType Name)
prettyLTyM (L srcloc ty) = do ty' <- prettyTyM ty
                              return $ L srcloc ty'

prettyTyM :: HsType Name -> State PrettyS (HsType Name)
prettyTyM (HsForAllTy e lbndrs lctxt lty) = do let bndrs = map unLoc lbndrs
                                                   lpreds = unLoc lctxt
                                               bndrs' <- mapM prettyTyVarBndrM bndrs
                                               lpreds' <- mapM prettyLPredM lpreds
                                               lty' <- prettyLTyM lty
                                               let lbndrs' = map genLoc bndrs'
                                                   lctxt' = genLoc lpreds'
                                               return $ HsForAllTy e lbndrs' lctxt' lty'
    where prettyTyVarBndrM (UserTyVar name) = do name' <- prettyNameM name
                                                 return $ UserTyVar name'                                                 
                                                        
          prettyPredM (HsClassP name ltys) = do ltys' <- mapM prettyLTyM ltys
                                                return $ HsClassP name ltys'

          prettyLPredM lpred = liftM genLoc $ prettyPredM $ unLoc lpred
                                                              
prettyTyM (HsTyVar name)                 = do name' <- prettyNameM name
                                              return $ HsTyVar name'                                                       
prettyTyM (HsBangTy bang lty)            = liftM (HsBangTy bang) $ prettyLTyM lty
prettyTyM (HsAppTy lt lu)                = do lt' <- prettyLTyM lt
                                              lu' <- prettyLTyM lu
                                              return $ HsAppTy lt' lu'
prettyTyM (HsFunTy lt lu)                = do lt' <- prettyLTyM lt
                                              lu' <- prettyLTyM lu
                                              return $ HsFunTy lt' lu'
prettyTyM (HsListTy lty)                 = liftM HsListTy $ prettyLTyM lty
prettyTyM (HsPArrTy lty)                 = liftM HsPArrTy $ prettyLTyM lty
prettyTyM (HsTupleTy boxity ltys)        = do ltys' <- mapM prettyLTyM ltys
                                              return $ HsTupleTy boxity ltys'                                                                                             
prettyTyM (HsParTy lty)                  = liftM HsParTy $ prettyLTyM lty
prettyTyM ty@(HsNumTy _)                 = return ty
-- prettyTyM (HsOpTy lleft lop lright)      = unsupported "HsOpTy"
-- prettyTyM (HsPredTy pred)                = unsupported "HsPredTy"
-- prettyTyM (HsKindSig lty kind)           = unsupported ["HsKindSig"]
-- prettyTyM (HsSpliceTy splice)            = unsupported ["HsSpliceTy"]
-- prettyTyM (HsDocTy lty ldoc)             = unsupported ["HsDocTy"]

runPretty s = evalState s mkPrettyS
prettyTy ty = runPretty (prettyTyM ty)
