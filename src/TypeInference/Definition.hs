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
  runCgen,
  freshVar,
  envInsert,
  emptySubst,
  subst,
  compose
) where

import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Except

type UVar = Int

data UType = Int | Bool | Char | Void
           | Prod UType UType | List UType
           | Fun [UType] UType
           | UVar UVar
  deriving Show

data UScheme = UScheme [UVar] UType

data Id = GlobalVar T.Text | LocalVar T.Text | FunName T.Text
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

runCgen :: CGen a -> Either T.Text a
runCgen x = fst <$> runExcept (runStateT
  x CGenState{ env = emptyEnv, varState = 0 })

freshVar :: CGen UVar
freshVar = do
  i <- gets varState
  modify (\s -> s{varState = i+1 })
  pure i

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


-- | Composes substition s1 after s2
compose :: Subst -> Subst -> Subst
compose s1 s2 = M.map (subst s1) s2 `M.union` s1
