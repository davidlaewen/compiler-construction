{-# LANGUAGE LambdaCase, OverloadedRecordDot, InstanceSigs #-}

module TypeInference.Definition (
  UVar,
  UType(..),
  UScheme(..),
  Subst,
  CGen,
  EnvLevel(..),
  Types(..),
  applySubst,
  runCgen,
  freshVar,
  emptySubst,
  envInsertVar,
  envInsertRetType,
  envLocalInsertFun,
  envGlobalInsertFun,
  clearLocalEnv,
  envLookupVar,
  envLookupFun,
  envLookupRetType,
  compose
) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import Data.Set ((\\))

type UVar = Int
type TVar = T.Text

data UType = Int | Bool | Char | Void
           | Prod UType UType | List UType
           | Fun [UType] UType
           | UVar UVar
           | TVar TVar
  deriving (Show, Eq)

-- freeTyVars :: UType -> S.Set TVar
-- freeTyVars Int = S.empty
-- freeTyVars Bool = S.empty
-- freeTyVars Char = S.empty
-- freeTyVars Void = S.empty
-- freeTyVars (Prod t1 t2) = freeTyVars t1 `S.union` freeTyVars t2
-- freeTyVars (List t) = freeTyVars t
-- freeTyVars (Fun ts t) = foldMap freeTyVars ts `S.union` freeTyVars t
-- freeTyVars (UVar _) = error "Called `freeTyVars` on a type containing uvars!"
-- freeTyVars (TVar s) = S.singleton s


data UScheme = UScheme (S.Set UVar) UType
  deriving (Show)

class Types a where
  subst :: Subst -> a -> a
  freeUVars :: a -> S.Set UVar

-- TODO: A single var sort might be sufficient, since scoping is determined
-- through modification of the CGen state
-- data Id = TermVar T.Text | FunName T.Text | TyVar TVar | RetType
--   deriving (Eq, Ord)

data LocalId = LocalTermVar T.Text | LocalFunName T.Text | TyVar TVar | RetType
  deriving (Eq, Ord)
data GlobalId = GlobalTermVar T.Text | GlobalFunName T.Text
  deriving (Eq, Ord)

data EnvLevel = GlobalLevel | LocalLevel

type GlobalEnv = M.Map GlobalId UScheme
type LocalEnv = M.Map LocalId UType

type VarState = UVar

type Subst = (M.Map UVar UType)

data CGenState = CGenState{ globalEnv :: GlobalEnv, localEnv :: LocalEnv, varState :: VarState }

type CGen = StateT CGenState (Except T.Text)

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

envInsertRetType :: UType -> CGen ()
envInsertRetType ty = modifyLocalEnv (M.insert RetType ty)

clearLocalEnv :: CGen ()
clearLocalEnv = modifyLocalEnv (const M.empty)

envLookupVar :: T.Text -> CGen (Maybe UType)
envLookupVar name =
  M.lookup (LocalTermVar name) <$> gets localEnv >>= \case
    Just ty -> pure (Just ty)
    Nothing ->
      M.lookup (GlobalTermVar name) <$> gets globalEnv >>= \case
        Just (UScheme binders ty) ->
          if S.null binders
            then pure (Just ty)
            else error "Found a variable with binders!!!"
        Nothing -> pure Nothing

envLookupFun :: T.Text -> CGen (Maybe UScheme)
envLookupFun name =
  M.lookup (LocalFunName name) <$> gets localEnv >>= \case
    Just ty -> pure (Just (UScheme S.empty ty))
    Nothing ->
      M.lookup (GlobalFunName name) <$> gets globalEnv

envLookupRetType :: CGen (Maybe UType)
envLookupRetType = gets (M.lookup RetType . localEnv)

-- envInsertFun :: EnvLevel -> T.Text -> UType -> CGen ()
-- envInsertVar GlobalLevel ident ty = modifyGlobalEnv (M.insert (GlobalTermVar ident) ty)
-- envInsertVar LocalLevel ident ty = modifyLocalEnv (M.insert (LocalTermVar ident) ty)

-- lookupGlobalEnv :: GlobalId -> CGen (Maybe UType)
-- lookupGlobalEnv name = do
--   globalEnv <- gets globalEnv
--   pure $ envLookup name globalEnv

-- lookupEnv :: Id -> CGen (Maybe UType)
-- lookupEnv name = do
--   localEnv <- gets localEnv
--   case envLookup name localEnv of
--     Just ty -> pure $ Just ty
--     Nothing -> lookupGlobalEnv name

runCgen :: CGen a -> Either T.Text a
runCgen x = fst <$> runExcept (runStateT
  x CGenState{ globalEnv = M.empty, localEnv = M.empty, varState = 0 })

freshVar :: CGen UVar
freshVar = do
  i <- gets varState
  modify (\s -> s{ varState = i+1 })
  pure i

-- envInsert :: Id -> UType -> Environment -> Environment
-- envInsert = M.insert

-- envMap :: (UType -> UType) -> Environment -> Environment
-- envMap = M.map

instance Types UType where
  subst :: Subst -> UType -> UType
  subst s (UVar i) =
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
  subst _ (TVar _) = error "Called `subst` on a user-defined type!"

  freeUVars :: UType -> S.Set UVar
  freeUVars Int = S.empty
  freeUVars Bool = S.empty
  freeUVars Char = S.empty
  freeUVars Void = S.empty
  freeUVars (Prod t1 t2) = freeUVars t1 `S.union` freeUVars t2
  freeUVars (List t) = freeUVars t
  freeUVars (Fun ts t) = foldMap freeUVars ts `S.union` freeUVars t
  freeUVars (UVar i) = S.singleton i
  freeUVars (TVar _) = S.empty

instance Types UScheme where
  subst :: Subst -> UScheme -> UScheme
  subst s (UScheme binders ty) = UScheme binders (subst (M.withoutKeys s binders) ty)

  freeUVars :: UScheme -> S.Set UVar
  freeUVars (UScheme binders ty) = freeUVars ty \\ binders



-- instance Types [UType] where
--   subst :: Subst -> [UType] -> [UType]
--   subst s = map (subst s)

--   freeUVars :: [UType]

-- TODO: Use Monoid for these two operations
emptySubst :: Subst
emptySubst = M.empty

-- | Composes substition s1 after s2
compose :: Subst -> Subst -> Subst
compose s1 s2 = M.map (subst s1) s2 `M.union` s1
