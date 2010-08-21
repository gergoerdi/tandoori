module Tandoori.Typing.Pretty(prettyTy, prettyTyM, prettyPolyTyM, runPretty) where

import Tandoori.GHC.Internals
import Tandoori
import Tandoori.Util
import Tandoori.Typing
    
import Data.Char
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map

type TvMap = Map.Map Tv Tv
data PrettyS = PrettyS { count :: Int,
                         tvmap :: TvMap }

mkPrettyS = PrettyS { count = 0, tvmap = Map.empty }
                 
isPrettyName :: Tv -> Bool
isPrettyName = not . isSystemName

prettyNameM :: Tv -> State PrettyS Tv
prettyNameM name | isPrettyName name = return name
                 | otherwise         = do s@PrettyS{tvmap = map, count = c} <- get
                                          case Map.lookup name map of
                                            Just name' -> return name'
                                            Nothing    -> do let name' = prettyName name c
                                                                 s' = s{tvmap = Map.insert name name' map, count = succ c}
                                                             put s'
                                                             return name'
    where prettyName name c = localiseName name'
              where name' = mkSysTvName u (mkFastString namestr)
                    u = nameUnique name
                    namestr | c < 26    = [chr $ (ord 'a') + c]
                            | otherwise = 't':(show (c - 26))

-- prettyNameM = undefined

-- prettyTyM :: Ty -> State PrettyS Ty
-- prettyTyM (HsForAllTy e _ lctxt lty) = do let lpreds = unLoc lctxt
--                                           lty' <- prettyLTyM lty
--                                           lpreds' <- mapM prettyLPredM lpreds
--                                           let lctxt' = genLoc lpreds'
--                                           return $ HsForAllTy e noBinder lctxt' lty'
--     where prettyPredM (HsClassP name ltys) = do ltys' <- mapM prettyLTyM ltys
--                                                 return $ HsClassP name ltys'

--           prettyLPredM lpred = liftM genLoc $ prettyPredM $ unLoc lpred

prettyTyM (TyVar α)     = liftM TyVar (prettyNameM α)
prettyTyM (TyFun τ1 τ2) = liftM2 TyFun (prettyTyM τ1) (prettyTyM τ2)
prettyTyM (TyApp τ1 τ2) = liftM2 TyApp (prettyTyM τ1) (prettyTyM τ2)
prettyTyM τ             = return τ

prettyPolyTyM (PolyTy ctx τ) = liftM2 PolyTy (mapM prettyPolyPredM ctx) (prettyTyM τ)

prettyPolyPredM (cls, α) = do α' <- prettyNameM α
                              return $ (cls, α')
                                    
runPretty s = evalState s mkPrettyS
prettyTy = runPretty . prettyTyM
