module Tandoori.GHC.Scope (runScope) where

import SrcLoc
import FastString    
import RdrName
import RdrHsSyn
import TcRnTypes
import RnSource
import RnNames
import Outputable
import GHC
import HscTypes
import TcRnMonad
import NameEnv
import NameSet
import VarSet
import Data.IORef
import qualified Data.Set as Set
import InstEnv
import FamInstEnv
import Module
import BasicTypes    
import Bag   

import Tandoori.Ty

builtinNames = builtinTyNames ++ builtinDataConNames ++ builtinClassNames
    
mkLcl = do errs_var <- newIORef (emptyBag, emptyBag)
           tvs_var  <- newIORef emptyVarSet
           return $ TcLclEnv {
                        tcl_errs       = errs_var,
                        tcl_loc        = mkGeneralSrcSpan (fsLit "Top level"),
                        tcl_ctxt       = [],
                        tcl_rdr        = emptyLocalRdrEnv `extendLocalRdrEnv` builtinNames,
                        tcl_th_ctxt    = topStage,
                        tcl_arrow_ctxt = NoArrowCtxt,
                        tcl_env        = emptyNameEnv,
                        tcl_tyvars     = tvs_var,
                        tcl_lie        = panic "initTc:LIE" -- only valid inside getLIE
                      }

mkGbl env mod = do dfuns_var    <- newIORef emptyNameSet ;
                   keep_var     <- newIORef emptyNameSet ;
                   used_rdrnames_var <- newIORef Set.empty ;
                   th_var       <- newIORef False ;
                   dfun_n_var   <- newIORef 0 ;
                   type_env_var <- case hsc_type_env_var env of
                                    Just (_mod, te_var) -> return te_var
                                    Nothing             -> newIORef emptyNameEnv
                   return $ TcGblEnv {
                                tcg_mod       = mod,
                                tcg_hmi       = emptyHaddockModInfo,
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
                                tcg_dus      = emptyDUs,
                                  
                                tcg_rn_imports = Just [],
                                tcg_rn_exports = Just [],
                                tcg_rn_decls   = Just emptyRnGroup,
                               
                                tcg_binds    = emptyLHsBinds,
                                
                                tcg_warns    = NoWarnings,
                                tcg_insts    = [],
                                tcg_fam_insts= [],
                                tcg_rules    = [],
                                tcg_fords    = [],
                                tcg_dfun_n   = dfun_n_var,
                                tcg_keep     = keep_var,
                                tcg_doc      = Nothing,
                                tcg_hpc      = False
                              }

runScope env lmod = do let modinfo = mkModule mainPackageId $ mkModuleName "foo"
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
       
