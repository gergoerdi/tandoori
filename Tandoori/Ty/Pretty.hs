module Tandoori.Ty.Pretty(prettifyTy) where

import HsTypes
import Data.Char
import SrcLoc
import Name
import FastString

import Control.Monad.State    
import qualified Data.Map as Map

type NameMap = Map.Map Name Name    
data PrettyS = PrettyS { count :: Int,
                         namemap :: NameMap }

mkPrettyS = PrettyS { count = 0, namemap = Map.empty }
                 
isPrettyName :: Name -> Bool
isPrettyName = not . isSystemName

prettifyNameM :: Name -> State PrettyS Name
prettifyNameM name | isPrettyName name = return name
                   | otherwise         = do s@PrettyS{namemap = map, count = c} <- get
                                            case Map.lookup name map of
                                              Just name' -> return name'
                                              Nothing    -> do let name' = prettifyName name c
                                                                   s' = s{namemap = Map.insert name name' map, count = c + 1}
                                                               put s'
                                                               return name'
    where prettifyName name c = localiseName name'
              where name' = mkSysTvName u (mkFastString namestr)
                    u = nameUnique name
                    namestr | c < 26    = [chr $ (ord 'a') + c]
                            | otherwise = 't':(show (c - 26))

prettifyLTyM :: LHsType Name -> State PrettyS (LHsType Name)
prettifyLTyM (L srcloc ty) = do ty' <- prettifyTyM ty
                                return $ L srcloc ty'

prettifyTyM :: HsType Name -> State PrettyS (HsType Name)
prettifyTyM (HsForAllTy e bndrs lctxt lty) = liftM unLoc $ prettifyLTyM lty -- TODO
prettifyTyM (HsTyVar name)                 = do name' <- prettifyNameM name
                                                return $ HsTyVar name'                                                       
prettifyTyM (HsBangTy bang lty)            = liftM (HsBangTy bang) $ prettifyLTyM lty
prettifyTyM (HsAppTy lt lu)                = do lt' <- prettifyLTyM lt
                                                lu' <- prettifyLTyM lu
                                                return $ HsAppTy lt' lu'
prettifyTyM (HsFunTy lt lu)                = do lt' <- prettifyLTyM lt
                                                lu' <- prettifyLTyM lu
                                                return $ HsFunTy lt' lu'
prettifyTyM (HsListTy lty)                 = liftM HsListTy $ prettifyLTyM lty
prettifyTyM (HsPArrTy lty)                 = liftM HsPArrTy $ prettifyLTyM lty
prettifyTyM (HsTupleTy boxity ltys)        = do ltys' <- mapM prettifyLTyM ltys
                                                return $ HsTupleTy boxity ltys'                                                                                             
prettifyTyM (HsParTy lty)                  = liftM HsParTy $ prettifyLTyM lty
prettifyTyM ty@(HsNumTy _)                 = return ty
-- prettifyTyM (HsOpTy lleft lop lright)      = unsupported "HsOpTy"
-- prettifyTyM (HsPredTy pred)                = unsupported "HsPredTy"
-- prettifyTyM (HsKindSig lty kind)           = unsupported ["HsKindSig"]
-- prettifyTyM (HsSpliceTy splice)            = unsupported ["HsSpliceTy"]
-- prettifyTyM (HsDocTy lty ldoc)             = unsupported ["HsDocTy"]

prettifyTy ty = evalState (prettifyTyM ty) mkPrettyS
