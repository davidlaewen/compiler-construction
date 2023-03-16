{-# LANGUAGE OverloadedStrings #-}

module TypeInference.Unify (
  Subst,
  unify,
  UType(..),
  UScheme(..),
  runCgen
) where
import TypeInference.Definition
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Except

unify :: UType -> UType -> CGen Subst
unify Int Int = pure M.empty
unify Bool Bool = pure M.empty
unify Char Char = pure M.empty
unify Void Void = pure M.empty
unify (UVar i) (UVar j) | i == j = pure M.empty
unify (UVar i) t =
  if occurs i t
  then throwError "Occurs check failed!"
  else pure $ M.singleton i t
unify t (UVar i) =
  if occurs i t
    then throwError "Occurs check failed!"
    else pure $ M.singleton i t
unify (List t1) (List t2) = unify t1 t2
unify (Prod s1 s2) (Prod t1 t2) = do
  subst1 <- unify s1 t1
  let s2' = subst subst1 s2
  let t2' = subst subst1 t2
  subst2 <- unify s2' t2'
  pure $ subst2 `compose` subst1
unify (Fun ts1 t1) (Fun ts2 t2) = unifyFunTypes (ts1,t1) (ts2,t2) M.empty
unify _ _ = throwError "Cannot unify types!"

unifyFunTypes :: ([UType], UType) -> ([UType], UType) -> Subst -> CGen Subst
unifyFunTypes ([],r1) ([],r2) acc = do
  s <- unify (subst acc r1) (subst acc r2)
  pure $ s `compose` acc
unifyFunTypes (t1:ts1,r1) (t2:ts2,r2) acc = do
  s <- unify (subst acc t1) (subst acc t2)
  unifyFunTypes (ts1,r1) (ts2,r2) (s `compose` acc)
unifyFunTypes _ _ _ = throwError "Function argument count mismatch!"

occurs :: UVar -> UType -> Bool
occurs v t = v `elem` freeVars t

freeVars :: UType -> S.Set UVar
freeVars Int = S.empty
freeVars Bool = S.empty
freeVars Char = S.empty
freeVars Void = S.empty
freeVars (Prod t1 t2) = S.union (freeVars t1) (freeVars t2)
freeVars (List t) = freeVars t
freeVars (Fun ts t) = S.union (foldMap freeVars ts) (freeVars t)
freeVars (UVar x) = S.singleton x
