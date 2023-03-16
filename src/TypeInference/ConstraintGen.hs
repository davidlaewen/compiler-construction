{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, InstanceSigs #-}

module TypeInference.ConstraintGen where

import TypeInference.Definition
import Syntax.TypeAST


class GenConstraints a where
  genConstraints :: a -> Subst

instance GenConstraints (Program ()) where
  genConstraints :: Program () -> Subst
  genConstraints (Program varDecls funDecls) = do
    genConstraints varDecls `compose` genConstraints funDecls

instance (GenConstraints f) => GenConstraints [f] where
  genConstraints :: [f] -> Subst
  genConstraints = foldr (\x y -> genConstraints x `compose` y) emptySubst

instance GenConstraints (VarDecl ()) where
  genConstraints (VarDecl mTy name expr) =
    case mTy of
      Just ty -> genConstraints expr
      Nothing -> genConstraints expr

instance GenConstraints (FunDecl ()) where

instance GenConstraints (Expr ()) where

instance GenConstraints (Stmt ()) where

instance GenConstraints VarLookup where
