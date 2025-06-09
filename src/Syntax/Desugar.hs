{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, InstanceSigs, OverloadedStrings #-}

module Syntax.Desugar (
  desugar
) where

import qualified Syntax.ParseAST as P
import qualified Syntax.TypeAST as T
import qualified TypeInference.Types as U ( UType(..) )
import Utils.Loc (Loc)
import qualified Control.Arrow as Data.Bifunctor

class Desugar a b where
  desugar :: a -> b

instance Desugar P.Program (T.Program () ()) where
  desugar :: P.Program -> T.Program () ()
  desugar (P.Program dataDecls varDecls funDecls) =
    T.Program (desugar <$> dataDecls) (desugar <$> varDecls) (T.SingleDecl . desugar <$> funDecls)

instance Desugar P.DataDecl T.DataDecl where
  desugar :: P.DataDecl -> T.DataDecl
  desugar (P.DataDecl loc name tyParams constrs) =
    T.DataDecl loc name tyParams (desugar <$> constrs)

instance Desugar P.DataConstr T.Ctor where
  desugar :: P.DataConstr -> T.Ctor
  desugar (P.DataConstr loc name args) =
    T.Ctor loc name $ Data.Bifunctor.second desugar <$> args

instance Desugar P.VarDecl (T.VarDecl ()) where
  desugar :: P.VarDecl -> T.VarDecl ()
  desugar (P.VarDecl loc mt name e) = T.VarDecl loc (desugar <$> mt) name (desugar e) ()


instance Desugar P.FunDecl (T.FunDecl () ()) where
  desugar :: P.FunDecl -> T.FunDecl () ()
  desugar (P.FunDecl loc name args rt varDecls stmts) =
    T.FunDecl loc name args (desugar <$> rt) (desugar <$> varDecls) (desugar <$> stmts) ()


instance Desugar P.Stmt (T.Stmt ()) where
  desugar :: P.Stmt -> T.Stmt ()
  desugar (P.If loc expr thenStmts elseStmts) =
    T.If loc (desugar expr) (desugar <$> thenStmts) (desugar <$> elseStmts)
  desugar (P.While loc e stmts) = T.While loc (desugar e) (desugar <$> stmts)
  desugar (P.Assign loc varLookup e) = T.Assign loc (desugar varLookup) () (desugar e)
  desugar (P.FunCall loc "print" args) = T.FunCall loc T.Print (desugar <$> args)
  desugar (P.FunCall loc "isEmpty" args) = T.FunCall loc T.IsEmpty (desugar <$> args)
  desugar (P.FunCall loc name args) = T.FunCall loc (T.Name name) (desugar <$> args)
  desugar (P.Return loc me) = T.Return loc (desugar <$> me)
  desugar P.GarbageStmt = error "Attempted to desugar garbage Stmt node!"


instance Desugar P.VarLookup T.VarLookup where
  desugar :: P.VarLookup -> T.VarLookup
  desugar (P.VarId loc name) = T.VarId loc name
  desugar (P.VarField loc varLookup field) =
    T.VarField loc (desugar varLookup) (desugar field)


instance Desugar P.Field T.Field where
  desugar :: P.Field -> T.Field
  desugar P.Head = T.Head
  desugar P.Tail = T.Tail
  desugar P.Fst = T.Fst
  desugar P.Snd = T.Snd
  desugar (P.Selector t) = T.SelField t
  desugar P.GarbageField = error "Attempted to desugar garbage Field node!"

instance Desugar P.Expr (T.Expr ()) where
  desugar :: P.Expr -> T.Expr ()
  desugar (P.Ident loc name) = T.Ident loc name ()
  desugar (P.ExprLookup loc exprLookup) = desugar (exprLookup, loc)
  desugar (P.Int loc i) = T.Int loc i ()
  desugar (P.Char loc c) = T.Char loc c ()
  desugar (P.Bool loc b) = T.Bool loc b ()
  desugar (P.FunCallE loc "print" args) = T.FunCallE loc T.Print (desugar <$> args) ()
  desugar (P.FunCallE loc "isEmpty" args) = T.FunCallE loc T.IsEmpty (desugar <$> args) ()
  desugar (P.FunCallE loc name args) = T.FunCallE loc (T.Name name) (desugar <$> args) ()
  desugar (P.ConstrCall loc name args) = T.FunCallE loc (T.CtorCall name) (desugar <$> args) ()
  desugar (P.EmptyList loc) = T.EmptyList loc ()
  desugar (P.Tuple loc e1 e2) = T.Tuple loc (desugar e1) (desugar e2) ()
  desugar P.GarbageExpr = error "Attempted to desugar garbage Expr node!"

  -- Unary operations
  desugar (P.UnOp loc P.Not e) = T.FunCallE loc T.Not [desugar e] ()
  desugar (P.UnOp loc P.Neg e) = T.FunCallE loc T.Neg [desugar e] ()

  -- Binary operations
  desugar (P.BinOp loc P.Add e1 e2) = T.FunCallE loc T.Add [desugar e1, desugar e2] ()
  desugar (P.BinOp loc P.Sub e1 e2) = T.FunCallE loc T.Sub [desugar e1, desugar e2] ()
  desugar (P.BinOp loc P.Mul e1 e2) = T.FunCallE loc T.Mul [desugar e1, desugar e2] ()
  desugar (P.BinOp loc P.Div e1 e2) = T.FunCallE loc T.Div [desugar e1, desugar e2] ()
  desugar (P.BinOp loc P.Mod e1 e2) = T.FunCallE loc T.Mod [desugar e1, desugar e2] ()

  desugar (P.BinOp loc P.Eq  e1 e2) = T.FunCallE loc T.Eq  [desugar e1, desugar e2] ()
  desugar (P.BinOp loc P.Neq e1 e2) = T.FunCallE loc T.Neq [desugar e1, desugar e2] ()
  desugar (P.BinOp loc P.Lt  e1 e2) = T.FunCallE loc T.Lt  [desugar e1, desugar e2] ()
  desugar (P.BinOp loc P.Gt  e1 e2) = T.FunCallE loc T.Gt  [desugar e1, desugar e2] ()
  desugar (P.BinOp loc P.Lte e1 e2) = T.FunCallE loc T.Lte [desugar e1, desugar e2] ()
  desugar (P.BinOp loc P.Gte e1 e2) = T.FunCallE loc T.Gte [desugar e1, desugar e2] ()

  desugar (P.BinOp loc P.And e1 e2)  = T.FunCallE loc T.And [desugar e1, desugar e2] ()
  desugar (P.BinOp loc P.Or e1 e2)   = T.FunCallE loc T.Or  [desugar e1, desugar e2] ()
  desugar (P.BinOp loc P.Cons e1 e2) = T.FunCallE loc T.Cons [desugar e1, desugar e2] ()


instance Desugar (P.ExprLookup, Loc) (T.Expr ()) where
  desugar :: (P.ExprLookup, Loc) -> T.Expr ()
  desugar (P.ExprField e P.Head, loc) = T.FunCallE loc T.HeadFun [desugar e] ()
  desugar (P.ExprField e P.Tail, loc) = T.FunCallE loc T.TailFun [desugar e] ()
  desugar (P.ExprField e P.Fst, loc)  = T.FunCallE loc T.FstFun  [desugar e] ()
  desugar (P.ExprField e P.Snd, loc)  = T.FunCallE loc T.SndFun  [desugar e] ()
  desugar (P.ExprField e (P.Selector t), loc) = T.FunCallE loc (T.Selector t) [desugar e] ()
  desugar (P.ExprField _ P.GarbageField, _) = error "Attempted to desugar garbage Field lookup!"


instance Desugar P.Type U.UType where
  desugar :: P.Type -> U.UType
  desugar (P.IntT  _) = U.Int
  desugar (P.BoolT _) = U.Bool
  desugar (P.CharT _) = U.Char
  desugar (P.Prod _ t1 t2) = U.Prod (desugar t1) (desugar t2)
  desugar (P.List _ t) = U.List (desugar t)
  desugar (P.Void _) = U.Void
  desugar (P.Fun _ ts t) = U.Fun (desugar <$> ts) (desugar t)
  desugar (P.DataT _ name tys) = U.Data name (desugar <$> tys)
  desugar (P.TyVar _ name) = U.TVar name
  desugar P.GarbageType = error "Attempted to desugar garbage Type node!"
