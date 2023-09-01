{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, InstanceSigs, OverloadedStrings #-}

module Syntax.Desugar (
  desugar
) where

import qualified Syntax.ParseAST as P
import qualified Syntax.TypeAST as T
import qualified TypeInference.Definition as U

class Desugar a b where
  desugar :: a -> b

instance Desugar P.Program (T.Program () ()) where
  desugar :: P.Program -> T.Program () ()
  desugar (P.Program varDecls funDecls) =
    T.Program (desugar <$> varDecls) (desugar <$> funDecls)


instance Desugar P.VarDecl (T.VarDecl ()) where
  desugar :: P.VarDecl -> T.VarDecl ()
  desugar (P.VarDecl mt name e) = T.VarDecl (desugar <$> mt) name (desugar e) ()


instance Desugar P.FunDecl (T.FunDecl () ()) where
  desugar :: P.FunDecl -> T.FunDecl () ()
  desugar (P.FunDecl name args rt varDecls stmts) =
    T.FunDecl name args (desugar <$> rt) (desugar <$> varDecls) (desugar <$> stmts) ()

instance Desugar P.FunMutDecl (T.FunMutDecl () ()) where
  desugar :: P.FunMutDecl -> T.FunMutDecl () ()
  desugar (P.MutualDecls funDecls) = T.MutualDecls $ desugar <$> funDecls
  desugar (P.SingleDecl funDecl) = T.SingleDecl $ desugar funDecl


instance Desugar P.Stmt (T.Stmt ()) where
  desugar :: P.Stmt -> T.Stmt ()
  desugar (P.If expr thenStmts elseStmts) =
    T.If (desugar expr) (desugar <$> thenStmts) (desugar <$> elseStmts)
  desugar (P.While e stmts) = T.While (desugar e) (desugar <$> stmts)
  desugar (P.Assign varLookup e) = T.Assign (desugar varLookup) () (desugar e)
  desugar (P.FunCall "print" args) = T.FunCall T.Print (desugar <$> args)
  desugar (P.FunCall "isEmpty" args) = T.FunCall T.IsEmpty (desugar <$> args)
  desugar (P.FunCall name args) = T.FunCall (T.Name name) (desugar <$> args)
  desugar (P.Return me) = T.Return (desugar <$> me)
  desugar P.GarbageS = error "Attempted to desugar GarbageS node from ParseAST!"


instance Desugar P.VarLookup T.VarLookup where
  desugar :: P.VarLookup -> T.VarLookup
  desugar (P.VarId name) = T.VarId name
  desugar (P.VarField varLookup field) =
    T.VarField (desugar varLookup) (desugar field)


instance Desugar P.Field T.Field where
  desugar :: P.Field -> T.Field
  desugar P.Head = T.Head
  desugar P.Tail = T.Tail
  desugar P.Fst = T.Fst
  desugar P.Snd = T.Snd


instance Desugar P.Expr (T.Expr ()) where
  desugar :: P.Expr -> T.Expr ()
  desugar (P.Ident name) = T.Ident name ()
  desugar (P.ExprLookup exprLookup) = desugar exprLookup
  desugar (P.Int i) = T.Int i ()
  desugar (P.Char c) = T.Char c ()
  desugar (P.Bool b) = T.Bool b ()
  desugar (P.FunCallE "print" args) = T.FunCallE T.Print (desugar <$> args) ()
  desugar (P.FunCallE "isEmpty" args) = T.FunCallE T.IsEmpty (desugar <$> args) ()
  desugar (P.FunCallE name args) = T.FunCallE (T.Name name) (desugar <$> args) ()
  desugar P.EmptyList = T.EmptyList ()
  desugar (P.Tuple e1 e2) = T.Tuple (desugar e1) (desugar e2) ()

  -- Unary operations
  desugar (P.UnOp P.Not e) = T.FunCallE T.Not [desugar e] ()
  desugar (P.UnOp P.Neg e) = T.FunCallE T.Neg [desugar e] ()

  -- Binary operations
  desugar (P.BinOp P.Add e1 e2) = T.FunCallE T.Add [desugar e1, desugar e2] ()
  desugar (P.BinOp P.Sub e1 e2) = T.FunCallE T.Sub [desugar e1, desugar e2] ()
  desugar (P.BinOp P.Mul e1 e2) = T.FunCallE T.Mul [desugar e1, desugar e2] ()
  desugar (P.BinOp P.Div e1 e2) = T.FunCallE T.Div [desugar e1, desugar e2] ()
  desugar (P.BinOp P.Mod e1 e2) = T.FunCallE T.Mod [desugar e1, desugar e2] ()

  desugar (P.BinOp P.Eq  e1 e2) = T.FunCallE T.Eq  [desugar e1, desugar e2] ()
  desugar (P.BinOp P.Neq e1 e2) = T.FunCallE T.Neq [desugar e1, desugar e2] ()
  desugar (P.BinOp P.Lt  e1 e2) = T.FunCallE T.Lt  [desugar e1, desugar e2] ()
  desugar (P.BinOp P.Gt  e1 e2) = T.FunCallE T.Gt  [desugar e1, desugar e2] ()
  desugar (P.BinOp P.Lte e1 e2) = T.FunCallE T.Lte [desugar e1, desugar e2] ()
  desugar (P.BinOp P.Gte e1 e2) = T.FunCallE T.Gte [desugar e1, desugar e2] ()

  desugar (P.BinOp P.And e1 e2) = T.FunCallE T.And [desugar e1, desugar e2] ()
  desugar (P.BinOp P.Or e1 e2)  = T.FunCallE T.Or  [desugar e1, desugar e2] ()
  desugar (P.BinOp P.Cons e1 e2) = T.FunCallE T.Cons [desugar e1, desugar e2] ()


instance Desugar P.ExprLookup (T.Expr ()) where
  desugar :: P.ExprLookup -> T.Expr ()
  desugar (P.ExprField expr P.Head) = T.FunCallE T.HeadFun [desugar expr] ()
  desugar (P.ExprField expr P.Tail) = T.FunCallE T.TailFun [desugar expr] ()
  desugar (P.ExprField expr P.Fst)  = T.FunCallE T.FstFun  [desugar expr] ()
  desugar (P.ExprField expr P.Snd)  = T.FunCallE T.SndFun  [desugar expr] ()


instance Desugar P.Type U.UType where
  desugar :: P.Type -> U.UType
  desugar P.IntT = U.Int
  desugar P.BoolT = U.Bool
  desugar P.CharT = U.Char
  desugar (P.Prod t1 t2) = U.Prod (desugar t1) (desugar t2)
  desugar (P.List t) = U.List (desugar t)
  desugar P.Void = U.Void
  desugar (P.Fun ts t) = U.Fun (desugar <$> ts) (desugar t)
  -- TODO: We probably want to replace named type variables with
  -- de Bruijn indices here
  desugar (P.TyVar name) = U.TVar name
  desugar P.GarbageT = error "Attempted to desugar GarbageT node from ParseAST!"
