{-# LANGUAGE LambdaCase #-}

module TypeInference.Definition (
  UVar,
  UType(..),
  UScheme(..),
  Environment,
  Subst,
  CGen,
  runCgen,
  freshVar,
  emptySubst,
  subst,
  compose
) where

import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

type UVar = Int

data UType = Int | Bool | Char | Void
           | Prod UType UType | List UType
           | Fun [UType] UType
           | UVar UVar
  deriving Show

data UScheme = UScheme [UVar] UType


type Environment = (M.Map T.Text UType)
type VarState = UVar

type Subst = (M.Map UVar UType)

type CGen a = ReaderT Environment (StateT VarState (Except T.Text)) a

runCgen :: CGen a -> Either T.Text a
runCgen x = fst <$> runExcept (runStateT (runReaderT x M.empty) 0)

freshVar :: CGen UVar
freshVar = do
  i <- get
  put $ i+1
  pure i

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
