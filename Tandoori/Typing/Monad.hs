{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns, ExistentialQuantification #-}
-- module Tandoori.Ty.State (Typing, runTyping,
--                           sinkWriter,
--                           mkTv,
--                           askCtxt, withCtxt,
--                           askCon, askBaseClassesOf, askBaseInstancesOf,
--                           askUserDecl, askPolyVar, askForcedMonoVars,
--                           withUserDecls, withMonoVars, withPolyVars,
--                           addError, withLoc, withSrc, withLSrc) where
module Tandoori.Typing.Monad where
-- module Tandoori.Errors (ErrorLocation(..), ErrorSource(..), ErrorMessage(..), ErrorContent(..), TyEq(..)) where

import Data.Maybe

import Tandoori
import Tandoori.GHC.Internals as GHC
import Tandoori.Typing
-- import Tandoori.Ty.ClassDecl
-- import Tandoori.Ty.InstanceDecl    
import Tandoori.Typing.Ctxt
import Tandoori.Typing.MonoEnv
import Control.Monad (liftM)
import Control.Monad.RWS (RWS, runRWS, asks, local, tell, gets, modify)
import Control.Monad.Writer (lift, runWriterT)
import Control.Monad.Error
import Control.Applicative
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
    
data R = R { loc :: SrcSpan,
             src :: Maybe ErrorSource,
                    
             kindmap :: KindMap,
             conmap :: ConMap,
             classmap :: ClsMap,
             instmap :: InstMap,
                        
             ctxt :: Ctxt }

newtype Typing a = Typing { unTyping :: ErrorT ErrorMessage (RWS R [ErrorMessage] Counter) a} deriving (Monad, Functor)

-- runTyping :: [LTyClDecl Name] -> [LInstDecl Name] -> Typing a -> (a, [ErrorMessage])
runTyping :: Typing a -> (Maybe a, [ErrorMessage])
runTyping typing = let (result, s', output) = (runRWS . runErrorT . unTyping) typing' r s
                   in case result of
                        Left err -> (Nothing, err:output)
                        Right result -> (Just result, output)
    where r = R { loc = noSrcSpan,
                  src = Nothing,
                  kindmap = Map.empty,
                  conmap = Map.empty,
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
withLoc loc = Typing . (local setLoc) . unTyping
    where setLoc r = r{loc}

withSrc :: Outputable e => e -> Typing a -> Typing a
withSrc src = Typing . (local setSrc) . unTyping
    where setSrc r = r{src = Just $ ErrorSource src}

withLSrc :: Outputable e => Located e -> Typing a -> Typing a
withLSrc (L loc src) = withLoc loc . withSrc src

addError :: ErrorContent -> Typing ()
addError err = do loc <- Typing $ asks loc
                  src <- Typing $ asks src
                  let msg = ErrorMessage (ErrorLocation loc src) err
                  Typing $ tell [msg]

throwErrorLOFASZ :: ErrorContent -> Typing a
throwErrorLOFASZ err = do loc <- Typing $ asks loc
                          src <- Typing $ asks src
                          let msg = ErrorMessage (ErrorLocation loc src) err
                          Typing $ throwError msg
                         
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










data ErrorSource = forall src. Outputable src => ErrorSource src                                  
data ErrorLocation = ErrorLocation SrcSpan (Maybe ErrorSource)
data ErrorMessage = ErrorMessage ErrorLocation ErrorContent

data ErrorContent = OtherMessage String
                  | UndefinedCon ConName
                  | UndefinedVar VarName
                  | UnificationFailed [MonoEnv] [TyEq]
                  | CantFitDecl PolyTy PolyTy [TyEq]
                  | AmbiguousPredicate PolyTy PolyPred
                  | UnfulfilledPredicate PolyPred
                  | OtherError String

instance Error ErrorContent where
    strMsg = OtherError
                    
instance Error ErrorMessage where
    strMsg = ErrorMessage (ErrorLocation noSrcSpan Nothing) . OtherError
                    


fromHsType :: GHC.HsType GHC.Name -> Typing PolyTy
fromHsType ty = do (τ, ctx) <- runWriterT $ fromHsType' ty
                   return $ PolyTy ctx τ
              
-- fromHsType' :: GHC.HsType GHC.Name -> WriterT PolyCtx (Either TypingError) Ty
fromHsType' τ@(GHC.HsTyVar name) | isTyCon τ  = return $ TyCon name
                                 | isTyVar τ  = return $ TyVar name
fromHsType' (GHC.HsFunTy lty1 lty2)           = do τ1 <- fromHsType' $ unLoc lty1
                                                   τ2 <- fromHsType' $ unLoc lty2
                                                   return $ TyFun τ1 τ2
fromHsType' (GHC.HsAppTy lty1 lty2)           = do τ1 <- fromHsType' $ unLoc lty1
                                                   τ2 <- fromHsType' $ unLoc lty2
                                                   return $ TyApp τ1 τ2
fromHsType' (GHC.HsListTy lty)                = tyList <$> fromHsType' (unLoc lty)
fromHsType' (GHC.HsTupleTy _ ltys)            = error "TODO: tuples"
fromHsType' (GHC.HsParTy lty)                 = fromHsType' (unLoc lty)
fromHsType' (GHC.HsDocTy lty _)               = fromHsType' (unLoc lty)
fromHsType' (GHC.HsOpTy lty1 (L _ op) lty2)   = do τ1 <- fromHsType' $ unLoc lty1
                                                   τ' <- fromHsType' $ GHC.HsTyVar op
                                                   τ2 <- fromHsType' $ unLoc lty2
                                                   return $ TyApp (TyApp τ' τ1) τ2
fromHsType' (GHC.HsForAllTy _ _ lctxt lty)    = do tell =<< mapM (toPolyPred . unLoc) (unLoc lctxt)
                                                   fromHsType' (unLoc lty)
    where toPolyPred (GHC.HsClassP cls [L _ τ@(GHC.HsTyVar tv)]) | isTyVar τ = return (cls, tv)
          toPolyPred (GHC.HsClassP cls [L _ τ]) = lift $ throwErrorLOFASZ $ strMsg "Malformed predicate"
          toPolyPred (GHC.HsClassP cls _)       = lift $ throwErrorLOFASZ $ strMsg "Predicate with more than one type parameter"
          toPolyPred _                          = lift $ throwErrorLOFASZ $ strMsg "Unsupported predicate"
                                                 
                                             
                                  
