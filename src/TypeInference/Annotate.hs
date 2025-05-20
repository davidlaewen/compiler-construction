module TypeInference.Annotate (annotateProgram) where

import TypeInference.Definition
import qualified Syntax.TypeAST as T

annotateProgram :: Subst -> T.Program UType UScheme -> T.Program UType UScheme
annotateProgram s (T.Program varDecls funDecls) =
  T.Program (annotateVarDecl s <$> varDecls) (annotateFunMutDecl s <$> funDecls)

annotateVarDecl :: Subst -> T.VarDecl UType -> T.VarDecl UType
annotateVarDecl s (T.VarDecl loc mTy name expr ty) =
  T.VarDecl loc mTy name (annotateExpr s expr) (subst s ty)

annotateFunMutDecl :: Subst -> T.FunMutDecl UType UScheme -> T.FunMutDecl UType UScheme
annotateFunMutDecl s (T.SingleDecl funDecl) = T.SingleDecl $ annotateFunDecl s funDecl
annotateFunMutDecl s (T.MutualDecls loc funDecls) = T.MutualDecls loc $ annotateFunDecl s <$> funDecls

annotateFunDecl :: Subst -> T.FunDecl UType UScheme -> T.FunDecl UType UScheme
annotateFunDecl s (T.FunDecl loc name params mTy varDecls stmts (UScheme _ ty)) =
  T.FunDecl loc name params mTy (annotateVarDecl s <$> varDecls) (annotateStmt s <$> stmts) funScheme
  where
    funTy = subst s ty
    funScheme = UScheme (freeUVars funTy) funTy

annotateStmt :: Subst -> T.Stmt UType -> T.Stmt UType
annotateStmt s (T.If loc condExpr thenStmts elseStmts) =
  T.If loc (annotateExpr s condExpr) (annotateStmt s <$> thenStmts) (annotateStmt s <$> elseStmts)
annotateStmt s (T.While loc condExpr loopStmts) =
  T.While loc (annotateExpr s condExpr) (annotateStmt s <$> loopStmts)
annotateStmt s (T.Assign loc varLookup varTy expr) =
  T.Assign loc varLookup (subst s varTy) (annotateExpr s expr)
annotateStmt s (T.FunCall loc funName args) =
  T.FunCall loc funName (annotateExpr s <$> args)
annotateStmt s (T.Return loc mExpr) =
  T.Return loc (annotateExpr s <$> mExpr)

annotateExpr :: Subst -> T.Expr UType -> T.Expr UType
annotateExpr s (T.Ident loc name ty) =
  T.Ident loc name (subst s ty)
annotateExpr _ (T.Int loc n Int) =
  T.Int loc n Int
annotateExpr _ (T.Int _ _ ty) =
  error $ "Integer has type " ++ show ty ++ " instead of Int"
annotateExpr _ (T.Char loc c Char) =
  T.Char loc c Char
annotateExpr _ (T.Char _ _ ty) =
  error $ "Character has type " ++ show ty ++ " instead of Char"
annotateExpr _ (T.Bool loc b Bool) =
  T.Bool loc b Bool
annotateExpr _ (T.Bool _ _ ty) =
  error $ "Boolean has type " ++ show ty ++ " instead of Bool"
annotateExpr s (T.FunCallE loc funName args ty) =
  T.FunCallE loc funName (annotateExpr s <$> args) (subst s ty)
annotateExpr s (T.EmptyList loc ty) =
  T.EmptyList loc (subst s ty)
annotateExpr s (T.Tuple loc e1 e2 ty) =
  T.Tuple loc (annotateExpr s e1) (annotateExpr s e2) (subst s ty)
