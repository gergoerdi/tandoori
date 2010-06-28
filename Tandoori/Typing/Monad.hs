{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns #-}
-- module Tandoori.Ty.State (Typing, runTyping,
--                           sinkWriter,
--                           mkTv,
--                           askCtxt, withCtxt,
--                           askCon, askBaseClassesOf, askBaseInstancesOf,
--                           askUserDecl, askPolyVar, askForcedMonoVars,
--                           withUserDecls, withMonoVars, withPolyVars,
--                           addError, withLoc, withSrc, withLSrc) where
module Tandoori.Typing.Monad where

import Tandoori
import Tandoori.Errors
import Tandoori.GHC.Internals
import Tandoori.Typing
-- import Tandoori.Ty.ClassDecl
-- import Tandoori.Ty.InstanceDecl    
import Tandoori.Typing.Ctxt
import Tandoori.Typing.MonoEnv
import Control.Monad (liftM)
import Control.Monad.RWS (RWS, runRWS, asks, local, tell, gets, modify)
import Control.Monad.Writer (lift, runWriterT)
import qualified Data.Set as Set
import qualified Data.Map as Map
    
import Debug.Trace

newtype Counter = Counter{ unCounter :: Int } deriving Enum

type KindMap = Map.Map TvName Int -- TODO: DataName
type ConMap = Map.Map ConName Ty
data ClsInfo = ClsInfo { clsSupers :: [Cls],
                         clsParam :: Tv,
                         clsVars :: Map.Map VarName PolyTy }    
type ClsMap = Map.Map Cls ClsInfo
type InstMap = Map.Map (Cls, TyCon) PolyTy    
    
data R = R { kindmap :: KindMap,
             conmap :: ConMap,
             classmap :: ClsMap,
             instmap :: InstMap,
             ctxt :: Ctxt }

newtype Typing a = Typing { unTyping :: RWS R [ErrorMessage] Counter a} deriving (Monad, Functor)

-- runTyping :: [LTyClDecl Name] -> [LInstDecl Name] -> Typing a -> (a, [ErrorMessage])
runTyping :: Typing a -> (a, [ErrorMessage])
runTyping typing = let (result, s', output) = (runRWS . unTyping) typing' r s
                   in (result, output)
    where r = R { kindmap = Map.empty,
                  conmap = Map.empty, -- TODO: built-in constructors for list and bool
                  classmap = Map.empty,
                  instmap = Map.empty,
                  ctxt = mkCtxt }
          s = Counter 0

          typing' = do α <- mkTyVar
                       let cons = map (\(n, τ) -> (dataConName n, τ)) $
                                  [(nilDataCon, tyList α),
                                   (consDataCon, tyCurryFun [α, tyList α, tyList α]),
                                   (trueDataCon, tyBool),
                                   (falseDataCon, tyBool)]
                       withCons cons $ typing              

fresh :: Typing Int
fresh = Typing $ do u <- gets unCounter
                    modify succ
                    return u
                      
mkTv :: Typing Tv
mkTv = do u <- fresh
          let namestr = 't':(show u)
              uname = mkAlphaTyVarUnique u
              name = mkSysTvName uname (mkFastString namestr)
          return name

mkTyVar :: Typing Ty
mkTyVar = liftM TyVar mkTv

withLoc :: SrcSpan -> Typing a -> Typing a
withLoc srcspan = Typing . (local setLoc) . unTyping
    where setLoc r@R{ctxt} = r{ ctxt = ctxt{loc = srcspan } }

withSrc :: Outputable e => e -> Typing a -> Typing a
withSrc src = Typing . (local setSrc) . unTyping
    where setSrc r@R{ctxt} = r{ ctxt = ctxt{ src = Just $ ErrorSource src } }

withLSrc :: Outputable e => Located e -> Typing a -> Typing a
withLSrc (L loc src) = withLoc loc . withSrc src

addError :: ErrorContent -> Typing ()
addError err = do loc <- Typing $ asks $ loc . ctxt
                  src <- Typing $ asks $ src . ctxt
                  let msg = ErrorMessage (ErrorLocation loc src) err
                  Typing $ tell [msg]

askCtxt = Typing $ asks ctxt                 
askForcedMonoVars = Typing $ asks $ monoVars . ctxt
askCon conname = liftM (Map.lookup conname) $ Typing $ asks conmap
-- askBaseClassesOf cls = do f <- Typing $ asks baseClasses
--                           return $ f cls

-- askBaseInstancesOf pred = do f <- Typing $ asks baseInstances
--                              return $ f pred

askUserDecl :: VarName -> Typing (Maybe (Located PolyTy))
askUserDecl varname = do c <- Typing $ asks ctxt
                         return $ getUserDecl c varname

askPolyVar :: VarName -> Typing (Maybe (MonoEnv, PolyTy))
askPolyVar varname = do c <- Typing $ asks $ ctxt
                        return $ getPolyVar c varname
               
withUserDecls :: [(VarName, Located PolyTy)] -> Typing a -> Typing a
withUserDecls binds = Typing . local add . unTyping
    where add r@R{ctxt} = r{ ctxt = addUserDecls ctxt binds }

withMonoVars :: Set.Set VarName -> Typing a -> Typing a
withMonoVars vars = Typing . local add . unTyping
    where add r@R{ctxt} = r{ ctxt = addMonoVars ctxt vars }

withPolyVars :: [(VarName, (MonoEnv, PolyTy))] -> Typing a -> Typing a
withPolyVars vars = Typing . local add . unTyping
    where add r@R{ctxt} = r{ ctxt = addPolyVars ctxt vars }
                              
withCtxt :: Ctxt -> Typing a -> Typing a
withCtxt ctxt = Typing . local change . unTyping
    where change r = r{ ctxt = ctxt }

sinkWriter xform writer = do (r, w) <- lift $ xform $ runWriterT writer
                             tell w
                             return r

withCons :: [(VarName, Ty)] -> Typing a -> Typing a
withCons cons = Typing . local add . unTyping
    where add r@R{conmap} = r{conmap = conmap `Map.union` (Map.fromList cons)}
