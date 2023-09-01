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
import Control.Monad.Except
import qualified Data.Text as T

unify :: UType -> UType -> CGen Subst
unify Int Int = pure mempty
unify Bool Bool = pure mempty
unify Char Char = pure mempty
unify Void Void = pure mempty
unify (UVar i) (UVar j) | i == j = pure mempty
unify (TVar t1) (TVar t2) | t1 == t2 = pure mempty
unify (UVar i) t =
  if occurs i t
  then throwError "Occurs check failed!"
  else pure $ Subst $ M.singleton i t
unify t (UVar i) =
  if occurs i t
  then throwError "Occurs check failed!"
  else pure $ Subst $ M.singleton i t
unify (List t1) (List t2) = unify t1 t2
unify (Prod s1 s2) (Prod t1 t2) = do
  subst1 <- unify s1 t1
  let s2' = subst subst1 s2
  let t2' = subst subst1 t2
  subst2 <- unify s2' t2'
  pure $ subst2 <> subst1
unify (Fun ts1 t1) (Fun ts2 t2) = unifyFunTypes (ts1,t1) (ts2,t2) mempty
unify ty1 ty2 = throwError $ "Cannot unify `" <> T.pack (show ty1) <> "` with `" <> T.pack (show ty2) <> "`"

unifyFunTypes :: ([UType], UType) -> ([UType], UType) -> Subst -> CGen Subst
unifyFunTypes ([],r1) ([],r2) acc = do
  s <- unify (subst acc r1) (subst acc r2)
  pure $ s <> acc
unifyFunTypes (t1:ts1,r1) (t2:ts2,r2) acc = do
  s <- unify (subst acc t1) (subst acc t2)
  unifyFunTypes (ts1,r1) (ts2,r2) (s <> acc)
unifyFunTypes _ _ _ = throwError "Function argument count mismatch!"

occurs :: UVar -> UType -> Bool
occurs v t = v `elem` freeUVars t
