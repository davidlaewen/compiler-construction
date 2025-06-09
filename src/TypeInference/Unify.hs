{-# LANGUAGE OverloadedStrings #-}

module TypeInference.Unify (
  Subst,
  unify,
  unifyLists,
  UType(..),
  UScheme(..)
) where
import TypeInference.Definition
import qualified Data.Map as M
import qualified Data.Text as T
import Utils.Loc (Loc)

-- | Perform unification of the given types, resulting in a substitution if the
-- types can be unified, and throws an error otherwise
unify :: UType -> UType -> Loc -- ^ The origin of the constraint
  -> CGen Subst
unify Int Int _ = pure mempty
unify Bool Bool _ = pure mempty
unify Char Char _ = pure mempty
unify Void Void _ = pure mempty
unify (UVar i) (UVar j) _ | i == j = pure mempty
unify (TVar t1) (TVar t2) _ | t1 == t2 = pure mempty
unify uv@(UVar i) t loc =
  if i `occursIn` t
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
unify ty1@(Fun ts1 rt1) ty2@(Fun ts2 rt2) loc = unifyFunTypes (ts1,rt1) (ts2,rt2) mempty loc
  -- if length ts1 /= length ts2 then throwLocError loc $
  --     "The number of input types in `" <> T.pack (show ty1) <>
  --     "` and `" <> T.pack (show ty2) <> "` do not match!"
  -- else do
  --   s <- unifyLists ts1 ts2 loc
  --   s' <- unify (subst s rt1) (subst s rt2) loc
  --   pure $ s' <> s
unify ty1@(Data t1 tys1) ty2@(Data t2 tys2) loc | t1 == t2 =
  if length tys1 /= length tys2 then throwLocError loc $
    "The number of type arguments in `" <> T.pack (show ty1) <>
    "` and `" <> T.pack (show ty2) <> "` do not match!"
  else unifyLists tys1 tys2 loc
unify ty1 ty2 loc = throwLocError loc $
  "Cannot unify `" <> T.pack (show ty1) <> "` with `" <> T.pack (show ty2) <> "`"

-- | Unify two lists of types. The client should check that the lists are of
-- equal length; if this function is called with lists of different length, it
-- will fail with internal error.
unifyLists :: [UType] -> [UType] -> Loc -> CGen Subst
unifyLists [] [] _ = pure mempty
unifyLists (ty1:tys1) (ty2:tys2) loc = do
  s <- unify ty1 ty2 loc
  ss <- unifyLists (subst s <$> tys1) (subst s <$> tys2) loc
  pure $ ss <> s
unifyLists _ _ _ = error "Tried to unify lists of differing length!"

unifyFunTypes :: ([UType], UType) -> ([UType], UType) -> Subst -> Loc -> CGen Subst
unifyFunTypes ([],r1) ([],r2) acc loc = do
  s <- unify (subst acc r1) (subst acc r2) loc
  pure $ s <> acc
unifyFunTypes (t1:ts1,r1) (t2:ts2,r2) acc loc = do
  s <- unify (subst acc t1) (subst acc t2) loc
  unifyFunTypes (ts1,r1) (ts2,r2) (s <> acc) loc
unifyFunTypes _ _ _ loc = throwLocError loc "Function argument count mismatch!"

occursIn :: UVar -> UType -> Bool
occursIn v t = v `elem` freeUVars t
