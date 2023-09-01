module TypeInference.Annotate (annotateProgram) where

import TypeInference.Definition
import qualified Syntax.TypeAST as T

annotateProgram :: Subst -> T.Program UType UScheme -> T.Program UType UScheme
annotateProgram s (T.Program varDecls funDecls) =
  T.Program (annotateVarDecl s <$> varDecls) (annotateFunMutDecl s <$> funDecls)

annotateVarDecl :: Subst -> T.VarDecl UType -> T.VarDecl UType
annotateVarDecl s (T.VarDecl mTy name expr ty) =
  T.VarDecl mTy name (annotateExpr s expr) (subst s ty)

annotateFunMutDecl :: Subst -> T.FunMutDecl UType UScheme -> T.FunMutDecl UType UScheme
annotateFunMutDecl s (T.SingleDecl funDecl) = T.SingleDecl $ annotateFunDecl s funDecl
annotateFunMutDecl s (T.MutualDecls funDecls) = T.MutualDecls $ annotateFunDecl s <$> funDecls

annotateFunDecl :: Subst -> T.FunDecl UType UScheme -> T.FunDecl UType UScheme
annotateFunDecl s (T.FunDecl name params mTy varDecls stmts ty) =
  T.FunDecl name params mTy (annotateVarDecl s <$> varDecls) (annotateStmt s <$> stmts) (subst s ty)

annotateStmt :: Subst -> T.Stmt UType -> T.Stmt UType
annotateStmt s (T.If condExpr thenStmts elseStmts) =
  T.If (annotateExpr s condExpr) (annotateStmt s <$> thenStmts) (annotateStmt s <$> elseStmts)
annotateStmt s (T.While condExpr loopStmts) =
  T.While (annotateExpr s condExpr) (annotateStmt s <$> loopStmts)
annotateStmt s (T.Assign varLookup varTy expr) =
  T.Assign varLookup (subst s varTy) (annotateExpr s expr)
annotateStmt s (T.FunCall funName args) =
  T.FunCall funName (annotateExpr s <$> args)
annotateStmt s (T.Return mExpr) =
  T.Return (annotateExpr s <$> mExpr)

annotateExpr :: Subst -> T.Expr UType -> T.Expr UType
annotateExpr s (T.Ident name ty) =
  T.Ident name (subst s ty)
annotateExpr _ (T.Int n Int) =
  T.Int n Int
annotateExpr _ (T.Int _ ty) = error $ "Integer has type " ++ show ty ++ " instead of Int"
annotateExpr _ (T.Char c Char) =
  T.Char c Char
annotateExpr _ (T.Char _ ty) = error $ "Character has type " ++ show ty ++ " instead of Char"
annotateExpr _ (T.Bool b Bool) =
  T.Bool b Bool
annotateExpr _ (T.Bool _ ty) = error $ "Boolean has type " ++ show ty ++ " instead of Bool"
annotateExpr s (T.FunCallE funName args ty) =
  T.FunCallE funName (annotateExpr s <$> args) (subst s ty)
annotateExpr s (T.EmptyList ty) =
  T.EmptyList (subst s ty)
annotateExpr s (T.Tuple e1 e2 ty) =
  T.Tuple (annotateExpr s e1) (annotateExpr s e2) (subst s ty)
