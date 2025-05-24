{-# LANGUAGE LambdaCase, OverloadedRecordDot, InstanceSigs, OverloadedStrings #-}

module TypeInference.Definition (
  TVar, UVar, UType(..),
  UScheme(..),
  Subst(Subst),
  CGen,
  EnvLevel(..),
  Types(..),
  throwLocError,
  applySubst,
  runCGen,
  freshVar, substTVars, freeTVars,
  envInsertVar, envInsertRetType,
  envLocalInsertFun, envGlobalInsertFun,
  envInsertCtor, envLookupCtor,
  envLookupSelector, envInsertSelector,
  clearLocalEnv,
  envLookupVar, envLookupFun,
  envLookupRetType,
) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import Data.Set ((\\))
import TypeInference.Types
import Syntax.TypeAST (FunDecl(..))
import Utils.Loc (Loc, locPretty)


class Types a where
  subst :: Subst -> a -> a
  freeUVars :: a -> S.Set UVar

data LocalId = LocalTermVar T.Text | LocalFunName T.Text | RetType
  deriving (Eq, Ord)

data GlobalId = GlobalTermVar T.Text | GlobalFunName T.Text
              | GlobalCtor T.Text | GlobalSelector T.Text
  deriving (Eq, Ord)

data EnvLevel = GlobalLevel | LocalLevel

type GlobalEnv = M.Map GlobalId UScheme
type LocalEnv = M.Map LocalId UType

type VarState = UVar

newtype Subst = Subst (M.Map UVar UType)

data CGenState = CGenState{ globalEnv :: GlobalEnv, localEnv :: LocalEnv, varState :: VarState }

type CGen = StateT CGenState (Except T.Text)

throwLocError :: Loc -> T.Text -> CGen a2
throwLocError loc msg = throwError $ T.pack (locPretty loc) <> ":\n" <> msg

applySubst :: Subst -> CGen ()
applySubst s = do
  modifyLocalEnv $ M.map (subst s)
  modifyGlobalEnv $ M.map (subst s)

modifyGlobalEnv :: (GlobalEnv -> GlobalEnv) -> CGen ()
modifyGlobalEnv f = modify (\s -> s{ globalEnv = f s.globalEnv })

modifyLocalEnv :: (LocalEnv -> LocalEnv) -> CGen ()
modifyLocalEnv f = modify (\s -> s{ localEnv = f s.localEnv })

envInsertVar :: EnvLevel -> T.Text -> UType -> CGen ()
envInsertVar GlobalLevel ident ty = modifyGlobalEnv (M.insert (GlobalTermVar ident) (UScheme S.empty ty))
envInsertVar LocalLevel ident ty = modifyLocalEnv (M.insert (LocalTermVar ident) ty)

envLocalInsertFun :: T.Text -> UType -> CGen ()
envLocalInsertFun ident ty = modifyLocalEnv (M.insert (LocalFunName ident) ty)

envGlobalInsertFun :: T.Text -> UScheme -> CGen ()
envGlobalInsertFun ident scheme = modifyGlobalEnv (M.insert (GlobalFunName ident) scheme)

envInsertCtor :: T.Text -> UScheme -> CGen ()
envInsertCtor name scheme = modifyGlobalEnv (M.insert (GlobalCtor name) scheme)

envInsertSelector :: T.Text -> UScheme -> CGen ()
envInsertSelector name scheme = modifyGlobalEnv (M.insert (GlobalSelector name) scheme)

envInsertRetType :: UType -> CGen ()
envInsertRetType ty = modifyLocalEnv (M.insert RetType ty)

clearLocalEnv :: CGen ()
clearLocalEnv = modifyLocalEnv (const M.empty)

envLookupVar :: T.Text -> CGen (Maybe UType)
envLookupVar name =
  gets (M.lookup (LocalTermVar name) . localEnv) >>= \case
    Just ty -> pure (Just ty)
    Nothing ->
      gets (M.lookup (GlobalTermVar name) . globalEnv) >>= \case
        Just (UScheme binders ty) ->
          if S.null binders
          then pure (Just ty)
          else error "Found a variable with binders!!!"
        Nothing -> pure Nothing

envLookupFun :: T.Text -> CGen (Maybe UScheme)
envLookupFun ident =
  gets (M.lookup (LocalFunName ident) . localEnv) >>= \case
    Just ty -> pure (Just (UScheme S.empty ty))
    Nothing ->
      gets (M.lookup (GlobalFunName ident) . globalEnv)

envLookupCtor :: T.Text -> CGen (Maybe UScheme)
envLookupCtor name = gets (M.lookup (GlobalCtor name) . globalEnv)

envLookupSelector :: T.Text -> CGen (Maybe UScheme)
envLookupSelector name = gets (M.lookup (GlobalSelector name) . globalEnv)


envLookupRetType :: CGen (Maybe UType)
envLookupRetType = gets (M.lookup RetType . localEnv)

runCGen :: CGen a -> Either T.Text a
runCGen x = fst <$> runExcept (runStateT
  x CGenState{ globalEnv = M.empty, localEnv = M.empty, varState = 0 })

freshVar :: CGen UVar
freshVar = do
  i <- gets varState
  modify (\s -> s{ varState = i+1 })
  pure i

-- | Replace type variables in user types with fresh unification variables
substTVars :: M.Map TVar UVar -> UType -> UType
substTVars s (TVar t) =
  case M.lookup t s of
    Nothing -> TVar t
    Just i -> UVar i
substTVars _ Int = Int
substTVars _ Bool = Bool
substTVars _ Char = Char
substTVars _ Void = Void
substTVars s (Prod t1 t2) = Prod (substTVars s t1) (substTVars s t2)
substTVars s (List t) = List (substTVars s t)
substTVars s (Fun ts t) = Fun (substTVars s <$> ts) (substTVars s t)
substTVars _ d@(Data _) = d
substTVars _ u@(UVar _) = u

freeTVars :: UType -> S.Set TVar
freeTVars Int = S.empty
freeTVars Bool = S.empty
freeTVars Char = S.empty
freeTVars Void = S.empty
freeTVars (Prod t1 t2) = freeTVars t1 `S.union` freeTVars t2
freeTVars (List t) = freeTVars t
freeTVars (Fun ts t) = foldMap freeTVars ts `S.union` freeTVars t
freeTVars (Data _) = S.empty
freeTVars (UVar _) = S.empty
freeTVars (TVar t) = S.singleton t

instance Types UType where
  subst :: Subst -> UType -> UType
  subst (Subst s) (UVar i) =
    case M.lookup i s of
      Nothing -> UVar i
      Just t -> t
  subst _ Int = Int
  subst _ Bool = Bool
  subst _ Char = Char
  subst _ Void = Void
  subst s (Prod t1 t2) = Prod (subst s t1) (subst s t2)
  subst s (List t) = List (subst s t)
  subst s (Fun ts t) = Fun (subst s <$> ts) (subst s t)
  subst _ d@(Data _) = d
  subst _ tv@(TVar _) = tv

  freeUVars :: UType -> S.Set UVar
  freeUVars Int = S.empty
  freeUVars Bool = S.empty
  freeUVars Char = S.empty
  freeUVars Void = S.empty
  freeUVars (Prod t1 t2) = freeUVars t1 `S.union` freeUVars t2
  freeUVars (List t) = freeUVars t
  freeUVars (Fun ts t) = foldMap freeUVars ts `S.union` freeUVars t
  freeUVars (Data _) = S.empty
  freeUVars (UVar i) = S.singleton i
  freeUVars (TVar _) = S.empty

instance Types UScheme where
  subst :: Subst -> UScheme -> UScheme
  -- subst (Subst s) (UScheme binders ty) = UScheme binders (subst (Subst $ M.withoutKeys s binders) ty)
  -- Substituting under the ∀ is fine, since all unification variables are unique.
  -- However, we need to update the bound variables to match the new body of the ∀-type.
  subst s (UScheme binders ty) = UScheme binders (subst s ty)

  freeUVars :: UScheme -> S.Set UVar
  freeUVars (UScheme binders ty) = freeUVars ty \\ binders

instance (Types b) => Types (FunDecl a b) where
  subst :: Subst -> FunDecl a b -> FunDecl a b
  subst s (FunDecl loc name params mTy varDecls stmts uScheme) =
    FunDecl loc name params mTy varDecls stmts (subst s uScheme)

  freeUVars :: FunDecl a b -> S.Set UVar
  freeUVars (FunDecl _ _ _ _ _ _ uScheme) = freeUVars uScheme

instance Semigroup Subst where
  Subst s1 <> Subst s2 = Subst $ M.map (subst (Subst s1)) s2 `M.union` s1

instance Monoid Subst where
  mempty = Subst M.empty
