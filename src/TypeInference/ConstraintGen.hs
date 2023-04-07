{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, InstanceSigs #-}

module TypeInference.ConstraintGen where

import TypeInference.Definition
import TypeInference.Unify
import qualified Syntax.TypeAST as T
import Control.Monad.State

checkProgram :: T.Program () -> CGen (T.Program UType, Subst)
checkProgram (T.Program varDecls funDecls) = do
  (vds,s1) <- checkVarDecls varDecls True
  (fds,s2) <- checkFunDecls funDecls
  pure (T.Program vds fds, s1 `compose` s2)

checkVarDecls :: [T.VarDecl ()] -> Bool -> CGen ([T.VarDecl UType], Subst)
checkVarDecls [] _ = pure ([], emptySubst)
checkVarDecls (varDecl:varDecls) isTopLevel = do
  (varDecl',s) <- checkVarDecl varDecl isTopLevel
  (varDecls',s') <- checkVarDecls varDecls isTopLevel
  pure (varDecl':varDecls', s' `compose` s)

checkVarDecl :: T.VarDecl () -> Bool -> CGen (T.VarDecl UType, Subst)
checkVarDecl (T.VarDecl mTy name expr _) isTopLevel = do
  uVar <- UVar <$> freshVar
  (expr', exprType, exprSubst) <- checkExpr expr
  modifyEnv $ envInsert (if isTopLevel then GlobalVar name else LocalVar name) uVar
  s <- unify exprType uVar
  {- TODO: Check inferred type against user-specified type
  case mTy of
    Just ty -> do
      sAnnot <- unify ty uVar
      ...
    Nothing -> pure (T.VarDecl mTy name expr' uVar, exprSubst)
  -}
  pure (T.VarDecl mTy name expr' uVar, s `compose` exprSubst)

checkFunDecls :: [T.FunDecl ()] -> CGen ([T.FunDecl UType], Subst)
checkFunDecls [] = pure ([], emptySubst)
checkFunDecls (funDecl:funDecls) = do
  (funDecl',s) <- checkFunDecl funDecl
  (funDecls',ss) <- checkFunDecls funDecls
  pure (funDecl':funDecls', ss `compose` s)

checkFunDecl :: T.FunDecl () -> CGen (T.FunDecl UType, Subst)
checkFunDecl (T.FunDecl name args mTy varDecls stmts _) = do
  uVarsArgs <- forM args (const $ UVar <$> freshVar)
  uVarRet <- UVar <$> freshVar
  globalState <- get
  modifyEnv (\env -> foldr (uncurry envInsert) env (zip (LocalVar <$> args) uVarsArgs))
  (varDecls',varDeclsSubst) <- checkVarDecls varDecls False
  (stmts',stmtsSubst) <- checkStmts stmts

  -- TODO: Add fun name to environment w/ inferred type

  put globalState

  pure (T.FunDecl name args mTy varDecls' stmts' (Fun uVarsArgs uVarRet), stmtsSubst `compose` varDeclsSubst)

checkStmts :: [T.Stmt ()] -> CGen ([T.Stmt UType], Subst)
checkStmts = _

checkStmt :: T.Stmt () -> CGen (T.Stmt UType, Subst)
checkStmt = _

checkExpr :: T.Expr () -> CGen (T.Expr UType, UType, Subst)
checkExpr (T.Ident name _) = do
  -- TODO: We need a way to check whether this a local or global variable
  mut <- lookupEnv (LocalVar name)
  case mut of
    Nothing -> error ""
    Just ut -> pure (T.Ident name ut, ut, emptySubst)
checkExpr (T.Int n _) = pure (T.Int n Int, Int, emptySubst)
checkExpr (T.Char c _) = pure (T.Char c Char, Char, emptySubst)
checkExpr (T.Bool b _) = pure (T.Bool b Bool, Bool, emptySubst)
checkExpr (T.FunCallE fname args _) = do
  _ -- TODO: Check arg list
  pure (T.FunCallE fname _ _, _, _)
checkExpr (T.EmptyList _) = pure (T.EmptyList _, _, emptySubst)
checkExpr (T.Tuple e1 e2 _) = do
  (e1',t1,s1) <- checkExpr e1
  (e2',t2,s2) <- checkExpr e2
  let ty = Prod t1 t2
  -- TODO: How to extract the types from e1' and e2' here?
  pure (T.Tuple e1' e2' ty, ty, s1 `compose` s2)
