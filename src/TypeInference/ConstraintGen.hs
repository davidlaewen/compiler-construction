{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, InstanceSigs #-}

module TypeInference.ConstraintGen where

import TypeInference.Definition
import TypeInference.Unify
import Syntax.TypeAST
import Control.Monad.State

checkProgram :: Program () -> CGen (Program UType, Subst)
checkProgram (Program varDecls funDecls) = do
  (vds,s1) <- checkVarDecls varDecls True
  (fds,s2) <- checkFunDecls funDecls
  pure (Program vds fds, s1 `compose` s2)

checkVarDecls :: [VarDecl ()] -> Bool -> CGen ([VarDecl UType], Subst)
checkVarDecls [] _ = pure ([], emptySubst)
checkVarDecls (varDecl:varDecls) isTopLevel = do
  (varDecl',s) <- checkVarDecl varDecl isTopLevel
  (varDecls',s') <- checkVarDecls varDecls isTopLevel
  pure (varDecl':varDecls', s' `compose` s)

checkVarDecl :: VarDecl () -> Bool -> CGen (VarDecl UType, Subst)
checkVarDecl (VarDecl mTy name expr _) isTopLevel = do
  uVar <- UVar <$> freshVar
  (expr',exprSubst) <- checkExpr expr
  modifyEnv $ envInsert (if isTopLevel then GlobalVar name else LocalVar name) uVar
  case mTy of
    Just ty -> do
      sAnnot <- unify ty uVar
      pure (VarDecl mTy name expr' uVar, sAnnot `compose` exprSubst)
    Nothing -> pure (VarDecl mTy name expr' uVar, exprSubst)

checkFunDecls :: [FunDecl ()] -> CGen ([FunDecl UType], Subst)
checkFunDecls = _

checkFunDecl :: FunDecl () -> CGen (FunDecl UType, Subst)
checkFunDecl (FunDecl name args mTy varDecls stmts _) = do
  uVarsArgs <- forM args (const $ UVar <$> freshVar)
  uVarRet <- UVar <$> freshVar
  globalState <- get
  modifyEnv (\env -> foldr (uncurry envInsert) env (zip (LocalVar <$> args) uVarsArgs))

  (varDecls',varDeclsSubst) <- checkVarDecls varDecls False

  (stmts',stmtsSubst) <- checkStmts stmts

  put globalState

  pure (FunDecl name args mTy varDecls' stmts' (Fun uVarsArgs uVarRet), _)

checkStmts :: [Stmt ()] -> CGen ([Stmt UType], Subst)
checkStmts = _

checkStmt :: Stmt () -> CGen (Stmt UType, Subst)
checkStmt = _

checkExpr :: Expr () -> CGen (Expr UType, Subst)
checkExpr = _
