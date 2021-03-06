{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns #-}
-- module Tandoori.Typing.Monad (ClsInfo(..), Typing, runTyping,
--                           mkTv,
--                           askCtxt, withCtxt,
--                           askCon, askClass, askSupers, askInstance,
--                           askUserDecl, askPolyVar, askForcedMonoVars,
--                           withCons, withClasses, withInstances,
--                           withUserDecls, withMonoVars, withPolyVars,
--                           raiseError, withLoc, withSrc, withLSrc) where
module Tandoori.Typing.Monad where

import Tandoori
import Tandoori.GHC.Internals
import Tandoori.Typing
import Tandoori.Typing.Error
import Tandoori.Typing.Ctxt
import Tandoori.Typing.MonoEnv
import Tandoori.Supply
import Control.Monad.RWS (RWS, runRWS, asks, local, tell, listen, censor)
import Control.Monad.Error
import Control.Applicative
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map    

--- Reader
type KindMap = Map TvName Int -- TODO: DataName
type ConMap = Map ConName Ty
data ClsInfo = ClsInfo { clsSupers :: [Cls],
                         clsParam :: Tv,
                         clsMeths :: Map VarName (Located PolyTy) }    
type ClsMap = Map Cls ClsInfo
type InstMap = Map (Cls, TyCon) PolyTy    
    
data R = R { loc :: SrcSpan,
             src :: Maybe SDoc,
                    
             kindmap :: KindMap,
             conmap :: ConMap,
             classmap :: ClsMap,
             instances :: InstMap,
                        
             ctxt :: Ctxt }

--- Writer
type VarSet = Set VarName
type W = ([ErrorMessage], VarSet)

--- The Typing monad       
newtype Typing a = Typing { unTyping :: ErrorT ErrorMessage (RWS R W (Supply Tv)) a} deriving (Monad, Functor)

runTyping :: Typing a -> (Maybe a, [ErrorMessage])
runTyping typing = let (result, s', (output, _)) = (runRWS . runErrorT . unTyping) typing' r s
                   in case result of
                        Left err -> (Nothing, err:output)
                        Right result -> (Just result, output)
    where r = R { loc = noSrcSpan,
                  src = Nothing,
                  kindmap = Map.empty,
                  conmap = Map.empty,
                  classmap = Map.empty,
                  instances = Map.empty,
                  ctxt = mkCtxt }
          s = Supply $ map toTv [1..]
            where toTv i = let name = 't':(show i)
                               uname = mkAlphaTyVarUnique i
                           in mkSysTvName uname (mkFastString name)                               

          typing' = do α <- mkTyVar
                       let cons = map (\(n, τ) -> (dataConName n, τ)) $
                                  [(nilDataCon, tyList α),
                                   (consDataCon, tyCurryFun [α, tyList α, tyList α]),
                                   (trueDataCon, tyBool),
                                   (falseDataCon, tyBool)]
                       withCons cons $ typing              

mkTv :: Typing Tv
mkTv = Typing getSupply

mkTyVar :: Typing Ty
mkTyVar = TyVar <$> mkTv

withLoc :: SrcSpan -> Typing a -> Typing a
withLoc loc = Typing . (local setLoc) . unTyping
    where setLoc r = r{loc}

withSrc :: Outputable e => e -> Typing a -> Typing a
withSrc src = Typing . (local setSrc) . unTyping
    where setSrc r = r{src = Just $ ppr src}

askSrc :: Typing (Maybe SDoc)
askSrc = Typing $ asks src

withLSrc :: Outputable e => Located e -> Typing a -> Typing a
withLSrc (L loc src) = withLoc loc . withSrc src

mkErrorMsg err = do loc <- Typing $ asks loc
                    src <- Typing $ asks src
                    return $ ErrorMessage (ErrorLocation loc src) err
                       
addError :: ErrorContent -> Typing ()
addError err = do msg <- mkErrorMsg err
                  tellErrors [msg]
                  
tellErrors :: [ErrorMessage] -> Typing ()                  
tellErrors msgs = Typing $ tell (msgs, mempty)
                       
tellVar :: VarName -> Typing ()
tellVar var = tellVars $ Set.singleton var
                      
tellVars :: VarSet -> Typing ()
tellVars vars = Typing $ tell (mempty, vars)

stopVars :: Typing a -> Typing a
stopVars = Typing . censor (fmap (const mempty)) . unTyping

listenVars :: Typing a -> Typing (a, VarSet)
listenVars f = Typing $ do (result, (_, vars)) <- listen $ unTyping f
                           return (result, vars)
                           
raiseError :: ErrorContent -> Typing a
raiseError err = do msg <- mkErrorMsg err
                    Typing $ throwError msg

orRecover :: Typing a -> Typing a -> Typing a
a `orRecover` b = Typing $ (unTyping a)
                  `catchError` (\err -> do                                     
                     unTyping $ tellErrors [err]
                     unTyping b)
                           
askCtxt = Typing $ asks ctxt
          
askForcedMonoVars = Typing $ asks $ monoVars . ctxt
                    
askCon name = do lookup <- Typing $ asks (Map.lookup name . conmap)
                 case lookup of
                   Nothing -> raiseError $ UndefinedCon name
                   Just con -> return con

askUserDecl :: VarName -> Typing (Maybe (Located PolyTy))
askUserDecl varname = do c <- Typing $ asks ctxt
                         return $ getUserDecl c varname

askPolyVar :: VarName -> Typing (Maybe (MonoEnv, Ty))
askPolyVar varname = do c <- Typing $ asks $ ctxt
                        return $ getPolyVar c varname
               
withUserDecls :: [(VarName, Located PolyTy)] -> Typing a -> Typing a
withUserDecls binds = Typing . local add . unTyping
    where add r@R{ctxt} = r{ ctxt = addUserDecls ctxt binds }

withMonoVars :: Set VarName -> Typing a -> Typing a
withMonoVars vars = Typing . local add . unTyping
    where add r@R{ctxt} = r{ ctxt = addMonoVars ctxt vars }

-- withPolyVars :: [(VarName, (MonoEnv, PolyTy))] -> Typing a -> Typing a
-- withPolyVars vars = Typing . local add . unTyping
--     where add r@R{ctxt} = r{ ctxt = addPolyVars ctxt vars }
                              
withCtxt :: Ctxt -> Typing a -> Typing a
withCtxt ctxt = Typing . local change . unTyping
    where change r = r{ ctxt = ctxt }

withCons :: [(VarName, Ty)] -> Typing a -> Typing a
withCons cons = Typing . local add . unTyping
    where add r@R{conmap} = r{conmap = conmap `Map.union` (Map.fromList cons)}

withClasses cis = withClassMap (Map.fromList cis) . withUserDecls vars 
    where vars = concatMap toVars cis
              where toVars (cls, ci) = map (fmap $ addClass (cls, α)) members
                        where α = clsParam ci
                              members = Map.toList $ clsMeths ci
              
          addClass (cls, α) (L loc (PolyTy ctx τ)) = L loc (PolyTy ctx' τ)
              where ctx' = (cls, α):ctx
          withClassMap classmap = Typing . local (\ ctx -> ctx{classmap}) . unTyping

withInstances is = Typing . local add . unTyping
    where add r@R{instances} = r{instances = instances `Map.union` (Map.fromList is)}

askInstance :: Cls -> TyCon -> Typing (Maybe PolyTy)
askInstance cls κ = Typing $ asks (Map.lookup (cls, κ) . instances)

askClass :: Cls -> Typing ClsInfo
askClass cls = do ci <- Typing $ asks (Map.lookup cls . classmap)
                  maybe (raiseError $ UndefinedCls cls) return ci
                    
askSupers :: Cls -> Typing [Cls]
askSupers cls = do ci <- askClass cls
                   clsSupers <$> askClass cls
