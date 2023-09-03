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
import qualified Data.Text as T
import Utils.Loc (Loc)

-- | Perform unification of the given types, resulting in a substitution if the
-- types can be unified
unify :: UType -> UType -> Loc -- ^ The origin of the constraint
  -> CGen Subst
unify Int Int _ = pure mempty
unify Bool Bool _ = pure mempty
unify Char Char _ = pure mempty
unify Void Void _ = pure mempty
unify (UVar i) (UVar j) _ | i == j = pure mempty
unify (TVar t1) (TVar t2) _ | t1 == t2 = pure mempty
unify uv@(UVar i) t loc =
  if occurs i t
  then throwLocError loc $ "Cannot solve constraint " <>
    T.pack (show uv) <> " ~ " <> T.pack (show t) <> ": Occurs check failed!"
  else pure $ Subst $ M.singleton i t
unify t uv@(UVar _) loc = unify uv t loc
unify (List t1) (List t2) loc = unify t1 t2 loc
unify (Prod s1 s2) (Prod t1 t2) loc = do
  subst1 <- unify s1 t1 loc
  let s2' = subst subst1 s2
  let t2' = subst subst1 t2
  subst2 <- unify s2' t2' loc
  pure $ subst2 <> subst1
unify (Fun ts1 t1) (Fun ts2 t2) loc = unifyFunTypes (ts1,t1) (ts2,t2) mempty loc
unify ty1 ty2 loc = throwLocError loc $
  "Cannot unify `" <> T.pack (show ty1) <> "` with `" <> T.pack (show ty2) <> "`"

unifyFunTypes :: ([UType], UType) -> ([UType], UType) -> Subst -> Loc -> CGen Subst
unifyFunTypes ([],r1) ([],r2) acc loc = do
  s <- unify (subst acc r1) (subst acc r2) loc
  pure $ s <> acc
unifyFunTypes (t1:ts1,r1) (t2:ts2,r2) acc loc = do
  s <- unify (subst acc t1) (subst acc t2) loc
  unifyFunTypes (ts1,r1) (ts2,r2) (s <> acc) loc
unifyFunTypes _ _ _ loc = throwLocError loc "Function argument count mismatch!"

occurs :: UVar -> UType -> Bool
occurs v t = v `elem` freeUVars t
