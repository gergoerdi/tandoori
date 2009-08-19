module Sandbox where

import Tandoori
import Tandoori.Test
import Tandoori.State    
import Tandoori.Ty
import Tandoori.Ty.Printer
import Tandoori.Scope
import Tandoori.CallGraph
import Control.Monad.State
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Haskell.Pretty
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
    
newtype MonoEnv = MonoEnv (Map.Map VarName HsType) deriving Show
data PolyEnv = PolyEnv { polyvarmap :: Map.Map VarName (MonoEnv, HsType),
                         conmap :: Map.Map ConName HsType,
                         monovars :: Set.Set VarName
                       } deriving Show

emptyMono :: MonoEnv
emptyMono = MonoEnv (Map.empty)

emptyPoly :: PolyEnv
emptyPoly = PolyEnv{polyvarmap = Map.empty, conmap = Map.empty, monovars = Set.empty}

-- TODO: rename            
addMonoVar :: MonoEnv -> VarName -> HsType -> MonoEnv
addMonoVar (MonoEnv m) name ty = MonoEnv (Map.insert name ty m)

getMonoVar :: MonoEnv -> VarName -> Maybe HsType
getMonoVar (MonoEnv m) name = Map.lookup name m
                                              
monoVars :: MonoEnv -> [(VarName, HsType)]
monoVars (MonoEnv m) = Map.toList m

combineMono :: MonoEnv -> MonoEnv -> MonoEnv
combineMono (MonoEnv m) (MonoEnv m') = MonoEnv $ Map.union m m'

combineMonos :: [MonoEnv] -> MonoEnv
combineMonos ms = foldl combineMono emptyMono ms
                                       

getCon :: PolyEnv -> ConName -> Maybe HsType
getCon PolyEnv{conmap = conmap} name = Map.lookup name conmap

getPolyVar :: PolyEnv -> VarName -> Maybe (MonoEnv, HsType)
getPolyVar PolyEnv{polyvarmap = polyvarmap} name = Map.lookup name polyvarmap

addPolyVar :: PolyEnv -> VarName -> (MonoEnv, HsType) -> PolyEnv
addPolyVar p@PolyEnv{polyvarmap = polyvarmap} name typing = p{polyvarmap = Map.insert name typing polyvarmap}
                                                   
removePolyVars :: PolyEnv -> [VarName] -> PolyEnv
removePolyVars p@PolyEnv{polyvarmap = polyvarmap} names = p{polyvarmap = foldl removePolyVar polyvarmap names}
    where removePolyVar = flip Map.delete
                                                   
isMonoVar :: PolyEnv -> VarName -> Bool
isMonoVar PolyEnv{monovars = monovars} name = Set.member name monovars

-- TODO: rename
addMonoVars :: PolyEnv -> [VarName] -> PolyEnv
addMonoVars p@PolyEnv{monovars = monovars} names = p{monovars = monovars `Set.union` (Set.fromList names)}

removeMonoVars :: MonoEnv -> [VarName] -> MonoEnv
removeMonoVars (MonoEnv m) names = MonoEnv $ foldl removeMonoVar m names
    where removeMonoVar = flip Map.delete

filterMonoVars :: MonoEnv -> (VarName -> HsType -> Bool) -> MonoEnv
filterMonoVars (MonoEnv m) p = MonoEnv $ Map.filterWithKey p m 
                          
tyCon :: PolyEnv -> HsQName -> Stateful HsType
tyCon p (UnQual name)                 = case getCon p name of
                                          Nothing -> do addError (UndefinedConstructor name)
                                                        createTv
                                          Just ty -> return ty
tyCon _ name | name == list_cons_name = do tv <- createTv
                                           return $ tyCurryFun [tv, tyList tv, tyList tv]


unify :: [MonoEnv] -> [HsType] -> Stateful (MonoEnv, HsType)
unify ms tys = do eqs <- monoeqs
                  alpha <- createTv
                  let eqs' = map (\ ty -> (alpha, ty)) tys
                  case mgu (eqs ++ eqs') of
                    Nothing -> error "Unification failed"
                    Just subst -> return (combineMonos (map (substMono subst) ms), substTy subst alpha)
                                  
    where monoeqs = do let vars = concat $ map monoVars ms
                           varnames = distinct $ map fst vars
                       tyvarmap <- liftM Map.fromList $ mapM (\ var -> do tv <- createTv; return (var, tv)) varnames
                       return $ map (\ (var, ty) -> (fromJust (Map.lookup var tyvarmap), ty)) vars


distinct = Set.toList . Set.fromList
                                  
type TvName = HsName              
newtype Substitution = S (Map.Map TvName HsType) deriving Show

emptySubst :: Substitution
emptySubst = S $ Map.empty

getSubst :: Substitution -> TvName -> Maybe HsType
getSubst (S m) name = Map.lookup name m

addSubst :: Substitution -> TvName -> HsType -> Substitution
addSubst (S m) name ty = S $ Map.insert name ty m
                      
substMono :: Substitution -> MonoEnv -> MonoEnv
substMono subst (MonoEnv m) = MonoEnv $ Map.map (substTy subst) m

substTy :: Substitution -> HsType -> HsType
substTy s (HsTyFun t t') = HsTyFun (substTy s t) (substTy s t')
substTy s (HsTyApp t t') = HsTyApp (substTy s t) (substTy s t')
substTy s ty@(HsTyVar name) = case getSubst s name of
                                Nothing -> ty
                                Just ty' -> substTy s ty'
substTy s ty = ty
                              
                                                  
    
    
mgu :: [(HsType, HsType)] -> Maybe Substitution
mgu [] = return emptySubst
mgu ((HsTyVar x,      HsTyVar y)     :eqs) | x == y  = mgu eqs
mgu ((HsTyVar x,      ty)            :eqs)           = if occurs ty
                                                       then (fail "occurs")
                                                       else do s <- mgu eqs'
                                                               return $ addSubst s x ty
    where occurs (HsTyVar y) | x == y = True
          occurs (HsTyFun t t') = occurs t || occurs t'
          occurs (HsTyApp t t') = occurs t || occurs t'
          occurs _ = False

          eqs' = map (\ (t, t') -> (subst t, subst t')) eqs
          subst = substTy (addSubst emptySubst x ty)
                     
mgu ((ty,           tv@(HsTyVar _)):eqs)           = mgu $ (tv, ty):eqs
mgu ((HsTyCon c,    HsTyCon c')    :eqs) | c == c' = mgu eqs
mgu ((HsTyFun t u,  HsTyFun t' u'):eqs)            = mgu $ (t, t'):(u, u'):eqs
mgu ((HsTyApp t u,  HsTyApp t' u'):eqs)            = mgu $ (t, t'):(u, u'):eqs
mgu ((HsTyTuple ts, HsTyTuple ts') :eqs)           = mgu $ (zip ts ts') ++ eqs
mgu ((l, r)                        :eqs)           = error $ unwords ["Unification failed at", show l, "=", show r] -- fail

maptupM :: (Monad m) => (a -> m (b, c)) -> [a] -> m ([b], [c])
maptupM action items = do results <- mapM action items
                          return (map fst results, map snd results)
                                                       
infer :: PolyEnv -> HsExp -> Stateful (MonoEnv, HsType)
infer p (HsLit lit) = return (emptyMono, typeOfLit lit)
infer p (HsList exprs) = do (ms, ts) <- maptupM (infer p) exprs
                            (m, t) <- unify ms ts
                            return (m, tyList t)
infer p (HsTuple exprs) = do (ms, ts) <- maptupM (infer p) exprs
                             return (combineMonos ms, tyTuple ts)
infer p (HsCon name) = do ty <- tyCon p name
                          return (emptyMono, ty)
infer p (HsApp fun param) = do (m1, ty1) <- infer p fun
                               (m2, ty2) <- infer p param
                               alpha <- createTv
                               (m, HsTyFun ty3 ty4) <- unify [m1, m2] [ty1, HsTyFun ty2 alpha]
                               return (m, ty4)
infer p (HsVar (UnQual name)) | isMonoVar p name = do alpha <- createTv
                                                      return $ (addMonoVar emptyMono name alpha, alpha)
                              | otherwise        = case getPolyVar p name of
                                                     Nothing -> error $ "Unknown variable " ++ (show name)
                                                     Just (m, t) -> return (m, t)
infer p (HsLambda srcloc pats expr) = withLoc srcloc $ do (ms, ts) <- maptupM (inferPat p') pats
                                                          let p'' = addMonoVars p' (map fst $ concat $ map monoVars ms)
                                                          (m, t) <- infer p'' expr
                                                          alpha <- createTv
                                                          beta <- createTv
                                                          (m', t') <- unify (m:ms) [HsTyFun (tyCurryFun ts) alpha, HsTyFun beta t]
                                                          let m'' = removeMonoVars m' monovars
                                                          return (m'', t')
    where monovars = concat $ map boundNamesOfPatInt pats
          p' = removePolyVars p monovars               
infer p (HsParen expr) = infer p expr
infer p (HsLet decls expr) = do --let declss = sortDecls decls
                                let newnamesInt = concat $ map boundNamesOfDeclInt decls
                                    newnamesExt = concat $ map boundNamesOfDeclExt decls
                                    p' = removePolyVars p newnamesInt
                                ms <- mapM (inferDef p') decls
                                (m, _) <- unify ms []
                                let m' = removeMonoVars m newnamesExt
                                    p'' = foldl (\ p name -> addPolyVar p name (reduce m' (fromJust $ getMonoVar m name))) p' newnamesExt
                                infer p'' expr
    where reduce :: MonoEnv -> HsType -> (MonoEnv, HsType)
          reduce m t = (m', t)
              where m' = let tyVars = tyVarsOf t
                         in filterMonoVars m (\ name ty -> Set.null $ (tyVarsOf ty) `Set.intersection` tyVars)
                    tyVarsOf :: HsType -> Set.Set HsName
                    tyVarsOf (HsTyVar x)          = Set.singleton x
                    tyVarsOf (HsTyFun left right) = (tyVarsOf left) `Set.union` (tyVarsOf right)
                    tyVarsOf (HsTyTuple tys)      = Set.unions $ map tyVarsOf tys
                    tyVarsOf (HsTyApp left right) = (tyVarsOf left) `Set.union` (tyVarsOf right)
                    tyVarsOf (HsTyCon _)          = Set.empty

inferRhs p (HsUnGuardedRhs expr) = infer p expr
inferRhs p (HsGuardedRhss rhss) = do (ms, ts) <- maptupM inferGuardedRhs rhss
                                     unify ms ts
    where inferGuardedRhs (HsGuardedRhs srcloc expr guard) = withLoc srcloc $ do (m, t) <- infer p expr
                                                                                 (m', t') <- infer p guard
                                                                                 (m'', t'') <- unify [m'] [t', tyBool]
                                                                                 unify [m, m''] [t]
                                     
inferDef :: PolyEnv -> HsDecl -> Stateful MonoEnv
inferDef p (HsPatBind srcloc pat rhs wheres) = withLoc srcloc $ do (m, t) <- inferPat p pat
                                                                   let p' = addMonoVars p (map fst $ monoVars m)
                                                                   (m', t') <- inferRhs p' rhs
                                                                   (m'', t'') <- unify [m, m'] [t, t']
                                                                   return m''
                                                                         
                         
inferPat :: PolyEnv -> HsPat -> Stateful (MonoEnv, HsType)
inferPat p (HsPVar name) = do alpha <- createTv
                              return (addMonoVar emptyMono name alpha, alpha)
inferPat p (HsPLit lit) = return (emptyMono, typeOfLit lit)
inferPat p (HsPApp conname pats) = do tycon <- tyCon p conname
                                      (ms, ts) <- maptupM (inferPat p) pats
                                      alpha <- createTv
                                      (m, t) <- unify ms [tycon, tyCurryFun (ts ++ [alpha])]
                                      return (m, last (tyUncurryFun t))
inferPat p (HsPTuple pats) = do (ms, ts) <- maptupM (inferPat p) pats
                                return (combineMonos ms, tyTuple ts)
inferPat p (HsPList pats) = do (ms, ts) <- maptupM (inferPat p) pats
                               unify ms ts                               
inferPat p (HsPAsPat name pat) = do (m, t) <- inferPat p pat
                                    return (addMonoVar m name t, t)
inferPat p (HsPWildCard) = do alpha <- createTv
                              return (emptyMono, alpha)
                                      

test expr = niceTy $ snd $ evalState (infer emptyPoly expr) newState
                                     
main = do expr <- getTestExpr
          return $ test expr

main' = evalState (inferPat emptyPoly pat) newState
    where pat = (HsPApp list_cons_name [HsPVar (HsIdent "x"), HsPVar (HsIdent "y")])
                   
ty1 = tyInt
ty2 = HsTyVar (HsSymbol "w")
ty3 = tyBool
ty4 = tyList ty1
      
mv1 = addMonoVar emptyMono (HsIdent "x") ty1
mv2 = addMonoVar emptyMono (HsIdent "x") ty2
mv2' = addMonoVar mv2 (HsIdent "y") tyInt
