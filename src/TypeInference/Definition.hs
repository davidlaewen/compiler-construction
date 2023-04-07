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
  modifyEnv,
  lookupEnv,
  runCgen,
  freshVar,
  envInsert,
  emptySubst,
  subst,
  compose
) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except

type UVar = Int
type TVar = T.Text

data UType = Int | Bool | Char | Void
           | Prod UType UType | List UType
           | Fun [UType] UType
           | UVar UVar
           | TVar TVar
  deriving Show

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


data UScheme = UScheme [UVar] UType

data Id = GlobalVar T.Text | LocalVar T.Text | FunName T.Text | TyVar TVar
  deriving (Eq, Ord)

type Environment = M.Map Id UType

emptyEnv :: Environment
emptyEnv = M.empty

type VarState = UVar

type Subst = (M.Map UVar UType)

data CGenState = CGenState{ env :: Environment, varState :: VarState }

type CGen a = (StateT CGenState (Except T.Text)) a

modifyEnv :: (Environment -> Environment) -> CGen ()
modifyEnv f = modify (\s -> s{ env = f s.env })

lookupEnv :: Id -> CGen (Maybe UType)
lookupEnv name = do
  env <- gets env
  pure $ envLookup name env

runCgen :: CGen a -> Either T.Text a
runCgen x = fst <$> runExcept (runStateT
  x CGenState{ env = emptyEnv, varState = 0 })

freshVar :: CGen UVar
freshVar = do
  i <- gets varState
  modify (\s -> s{varState = i+1 })
  pure i

envLookup :: Id -> Environment -> Maybe UType
envLookup = M.lookup

envInsert :: Id -> UType -> Environment -> Environment
envInsert = M.insert

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
