module Tandoori.Typing.Pretty(prettyTvM, prettyTy, prettyTyM, prettyPolyTyM, runPretty) where

import Tandoori.GHC.Internals
import Tandoori.Typing
import Tandoori.Supply    

import Data.Char
import Control.Monad.State
import qualified Data.Map as Map

type TvMap = Map.Map Tv Tv
type PrettyM a = StateT TvMap (State (Supply FastString)) a

isPrettyName :: Tv -> Bool
isPrettyName = not . isSystemName

prettyTvM :: Tv -> PrettyM Tv
prettyTvM α | isPrettyName α = return α
            | otherwise      = do lookup <- gets $ Map.lookup α
                                  case lookup of
                                    Just α' -> return α'
                                    Nothing  -> do name <- lift getSupply
                                                   let α' = mkSysTvName (nameUnique α) name
                                                   modify $ Map.insert α α'
                                                   return α'
                                                     
prettyTyM (TyVar α)     = liftM TyVar (prettyTvM α)
prettyTyM (TyFun τ1 τ2) = liftM2 TyFun (prettyTyM τ1) (prettyTyM τ2)
prettyTyM (TyApp τ1 τ2) = liftM2 TyApp (prettyTyM τ1) (prettyTyM τ2)
prettyTyM τ             = return τ

prettyPolyTyM (PolyTy ctx τ) = liftM2 PolyTy (mapM prettyPolyPredM ctx) (prettyTyM τ)

prettyPolyPredM (cls, α) = do α' <- prettyTvM α
                              return $ (cls, α')
                                    
runPretty p = evalState (evalStateT p Map.empty) (Supply αs)
  where αs = map (mkFastString . nameStr) [0..]
        nameStr c | c < 26 = [chr $ (ord 'a') + c]
                  | otherwise = 't':show (c - 26)
prettyTy = runPretty . prettyTyM
