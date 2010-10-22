module Tandoori.GHC.Scope (runScope) where

import SrcLoc (mkGeneralSrcSpan, unLoc, Located(..))
import FastString (fsLit)
import RdrName (RdrName, emptyLocalRdrEnv, extendLocalRdrEnvList)
import RdrHsSyn (findSplice)
import RnSource (rnSrcDecls, rnTyClDecls)
import Panic (panic)
import GHC (emptyRnGroup, emptyLHsBinds, mkModule, mkModuleName, HsModule(..), HsDecl(..))
import HscTypes (HscEnv, hsc_global_rdr_env, hsc_global_type_env, hsc_type_env_var, Warnings(..))
import HsDecls (LTyClDecl, HsGroup)
import DriverPhases (HscSource(..))
import TcRnMonad (TcGblEnv(..), TcLclEnv(..), initTcRnIf)
import TcRnTypes (ArrowCtxt (..), RecFieldEnv(..), topStage, emptyImportAvails)
import Name (Name)
import NameEnv (emptyNameEnv)
import NameSet (emptyNameSet, emptyDUs)
import VarSet (emptyVarSet)
import Data.IORef (newIORef)
import qualified Data.Set as Set (empty)
import InstEnv (emptyInstEnv)
import FamInstEnv (emptyFamInstEnv)
import Module (mainPackageId)
import Bag (emptyBag)
import OccName (emptyOccSet)
import HsImpExp (LImportDecl)
    
import Tandoori.Typing

builtinNames = builtinTyNames ++ builtinDataConNames ++ builtinClassNames
    
mkLcl = do errs_var <- newIORef (emptyBag, emptyBag)
           tvs_var  <- newIORef emptyVarSet
           return $ TcLclEnv {
                        tcl_errs       = errs_var,
                        tcl_loc        = mkGeneralSrcSpan (fsLit "Top level"),
                        tcl_ctxt       = [],
                        tcl_rdr        = emptyLocalRdrEnv `extendLocalRdrEnvList` builtinNames,
                        tcl_th_ctxt    = topStage,
                        tcl_arrow_ctxt = NoArrowCtxt,
                        tcl_env        = emptyNameEnv,
                        tcl_tyvars     = tvs_var,
                        tcl_lie        = panic "tcl_lie",
                        tcl_tybinds    = panic "tcl_tybinds"
                      }

mkGbl env mod = do dfuns_var         <- newIORef emptyNameSet
                   keep_var          <- newIORef emptyNameSet
                   used_rdrnames     <- newIORef Set.empty
                   th_var            <- newIORef False
                   dfun_n_var        <- newIORef emptyOccSet
                   type_env_var      <- case hsc_type_env_var env of
                                         Just (_mod, te_var) -> return te_var
                                         Nothing             -> newIORef emptyNameEnv
                   return $ TcGblEnv {
                                tcg_mod       = mod,
                                --tcg_hmi       = emptyHaddockModInfo,
                                tcg_src       = HsSrcFile,
                                tcg_rdr_env   = hsc_global_rdr_env env,
                                tcg_fix_env   = emptyNameEnv,
                                tcg_field_env = RecFields emptyNameEnv emptyNameSet,
                                tcg_default   = Nothing,
                                tcg_type_env  = hsc_global_type_env env,
                                tcg_type_env_var = type_env_var,
                                tcg_inst_env  = emptyInstEnv,
                                tcg_fam_inst_env  = emptyFamInstEnv,
                                tcg_inst_uses = dfuns_var,
                                tcg_th_used   = th_var,
                                tcg_exports  = [],
                                tcg_imports  = emptyImportAvails,
                                tcg_used_rdrnames = used_rdrnames,
                                tcg_dus      = emptyDUs,
                                  
                                tcg_rn_imports = [],
                                tcg_rn_exports = Just [],
                                tcg_rn_decls   = Just emptyRnGroup,
                               
                                tcg_binds    = emptyLHsBinds,                                
                                tcg_warns    = NoWarnings,
                                tcg_anns     = [],
                                tcg_insts    = [],
                                tcg_fam_insts= [],
                                tcg_rules    = [],
                                tcg_fords    = [],
                                tcg_dfun_n   = dfun_n_var,
                                tcg_keep     = keep_var,
                                tcg_doc_hdr  = Nothing,
                                tcg_hpc      = False
                              }

runScope :: HscEnv -> Located (HsModule RdrName) -> IO ([LImportDecl Name], [LTyClDecl Name], HsGroup Name)
runScope env lmod = do let modinfo = mkModule mainPackageId $ mkModuleName "Main"
                           group = fst $ findSplice decls
                       gbl <- mkGbl env modinfo
                       lcl <- mkLcl

                       -- (rn_imports, rdr_env, _, _) <- initTcRnIf 'a' env gbl lcl $ checkNoErrs $ rnImports imports
                                                                 
                       (gbl', group') <- initTcRnIf 'a' env gbl lcl $ rnSrcDecls group
                       tydecls' <- initTcRnIf 'a' env gbl' lcl $ rnTyClDecls ltycldecls
                       return (undefined, tydecls', group')
                              
    where mod = unLoc lmod
          imports = hsmodImports mod
          decls = hsmodDecls mod
          tycldecls = filter (isTyDecl . unLoc) decls
          ltycldecls = map (\ (L loc (TyClD decl)) -> L loc decl) tycldecls
          isTyDecl (TyClD _) = True
          isTyDecl _ = False
       
