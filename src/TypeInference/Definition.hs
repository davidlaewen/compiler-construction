{-# LANGUAGE LambdaCase, OverloadedRecordDot #-}

module TypeInference.Definition (
  UVar,
  UType(..),
  UScheme(..),
  Id(..),
  Environment,
  emptyEnv,
  Subst,
  CGen,
  modifyGlobalEnv,
  modifyLocalEnv,
  lookupGlobalEnv,
  lookupEnv,
  runCgen,
  freshVar,
  envInsert,
  envMap,
  emptySubst,
  subst,
  compose
) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

type UVar = Int
type TVar = T.Text

data UType = Int | Bool | Char | Void
           | Prod UType UType | List UType
           | Fun [UType] UType
           | UVar UVar
           | TVar TVar
  deriving (Show, Eq)

freeTyVars :: UType -> S.Set TVar
freeTyVars Int = S.empty
freeTyVars Bool = S.empty
freeTyVars Char = S.empty
freeTyVars Void = S.empty
freeTyVars (Prod t1 t2) = freeTyVars t1 `S.union` freeTyVars t2
freeTyVars (List t) = freeTyVars t
freeTyVars (Fun ts t) = foldMap freeTyVars ts `S.union` freeTyVars t
freeTyVars (UVar _) = error "Called `freeTyVars` on a type containing uvars!"
freeTyVars (TVar s) = S.singleton s


data UScheme = UScheme [TVar] UType

-- TODO: A single var sort might be sufficient, since scoping is determined
-- through modification of the CGen state
data Id = TermVar T.Text | FunName T.Text | TyVar TVar
  deriving (Eq, Ord)

type Environment = M.Map Id UType

emptyEnv :: Environment
emptyEnv = M.empty

type VarState = UVar

type Subst = (M.Map UVar UType)

data CGenState = CGenState{ globalEnv :: Environment, localEnv :: Environment, varState :: VarState }

type CGen a = (ReaderT (Maybe UVar) (StateT CGenState (Except T.Text))) a

modifyGlobalEnv :: (Environment -> Environment) -> CGen ()
modifyGlobalEnv f = modify (\s -> s{ globalEnv = f s.globalEnv })

modifyLocalEnv :: (Environment -> Environment) -> CGen ()
modifyLocalEnv f = modify (\s -> s{ localEnv = f s.localEnv })

lookupGlobalEnv :: Id -> CGen (Maybe UType)
lookupGlobalEnv name = do
  globalEnv <- gets globalEnv
  pure $ envLookup name globalEnv

lookupEnv :: Id -> CGen (Maybe UType)
lookupEnv name = do
  localEnv <- gets localEnv
  case envLookup name localEnv of
    Just ty -> pure $ Just ty
    Nothing -> lookupGlobalEnv name

runCgen :: CGen a -> Either T.Text a
runCgen x = fst <$> runExcept (runStateT
  (runReaderT x Nothing) CGenState{ globalEnv = emptyEnv, localEnv = emptyEnv, varState = 0 })

freshVar :: CGen UVar
freshVar = do
  i <- gets varState
  modify (\s -> s{ varState = i+1 })
  pure i

envLookup :: Id -> Environment -> Maybe UType
envLookup = M.lookup

envInsert :: Id -> UType -> Environment -> Environment
envInsert = M.insert

envMap :: (UType -> UType) -> Environment -> Environment
envMap = M.map

emptySubst :: Subst
emptySubst = M.empty

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


-- | Composes substition s1 after s2
compose :: Subst -> Subst -> Subst
compose s1 s2 = M.map (subst s1) s2 `M.union` s1
