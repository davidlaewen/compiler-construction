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
  desugar (P.VarDecl _ mt name e) = T.VarDecl (desugar <$> mt) name (desugar e) ()


instance Desugar P.FunDecl (T.FunDecl () ()) where
  desugar :: P.FunDecl -> T.FunDecl () ()
  desugar (P.FunDecl _ name args rt varDecls stmts) =
    T.FunDecl name args (desugar <$> rt) (desugar <$> varDecls) (desugar <$> stmts) ()

instance Desugar P.FunMutDecl (T.FunMutDecl () ()) where
  desugar :: P.FunMutDecl -> T.FunMutDecl () ()
  desugar (P.MutualDecls _ funDecls) = T.MutualDecls $ desugar <$> funDecls
  desugar (P.SingleDecl funDecl) = T.SingleDecl $ desugar funDecl


instance Desugar P.Stmt (T.Stmt ()) where
  desugar :: P.Stmt -> T.Stmt ()
  desugar (P.If _ expr thenStmts elseStmts) =
    T.If (desugar expr) (desugar <$> thenStmts) (desugar <$> elseStmts)
  desugar (P.While _ e stmts) = T.While (desugar e) (desugar <$> stmts)
  desugar (P.Assign _ varLookup e) = T.Assign (desugar varLookup) () (desugar e)
  desugar (P.FunCall _ "print" args) = T.FunCall T.Print (desugar <$> args)
  desugar (P.FunCall _ "isEmpty" args) = T.FunCall T.IsEmpty (desugar <$> args)
  desugar (P.FunCall _ name args) = T.FunCall (T.Name name) (desugar <$> args)
  desugar (P.Return _ me) = T.Return (desugar <$> me)
  desugar P.GarbageS = error "Attempted to desugar GarbageS node from ParseAST!"


instance Desugar P.VarLookup T.VarLookup where
  desugar :: P.VarLookup -> T.VarLookup
  desugar (P.VarId _ name) = T.VarId name
  desugar (P.VarField _ varLookup field) =
    T.VarField (desugar varLookup) (desugar field)


instance Desugar P.Field T.Field where
  desugar :: P.Field -> T.Field
  desugar P.Head = T.Head
  desugar P.Tail = T.Tail
  desugar P.Fst = T.Fst
  desugar P.Snd = T.Snd


instance Desugar P.Expr (T.Expr ()) where
  desugar :: P.Expr -> T.Expr ()
  desugar (P.Ident _ name) = T.Ident name ()
  desugar (P.ExprLookup _ exprLookup) = desugar exprLookup
  desugar (P.Int _ i) = T.Int i ()
  desugar (P.Char _ c) = T.Char c ()
  desugar (P.Bool _ b) = T.Bool b ()
  desugar (P.FunCallE _ "print" args) = T.FunCallE T.Print (desugar <$> args) ()
  desugar (P.FunCallE _ "isEmpty" args) = T.FunCallE T.IsEmpty (desugar <$> args) ()
  desugar (P.FunCallE _ name args) = T.FunCallE (T.Name name) (desugar <$> args) ()
  desugar (P.EmptyList _) = T.EmptyList ()
  desugar (P.Tuple _ e1 e2) = T.Tuple (desugar e1) (desugar e2) ()

  -- Unary operations
  desugar (P.UnOp _ P.Not e) = T.FunCallE T.Not [desugar e] ()
  desugar (P.UnOp _ P.Neg e) = T.FunCallE T.Neg [desugar e] ()

  -- Binary operations
  desugar (P.BinOp _ P.Add e1 e2) = T.FunCallE T.Add [desugar e1, desugar e2] ()
  desugar (P.BinOp _ P.Sub e1 e2) = T.FunCallE T.Sub [desugar e1, desugar e2] ()
  desugar (P.BinOp _ P.Mul e1 e2) = T.FunCallE T.Mul [desugar e1, desugar e2] ()
  desugar (P.BinOp _ P.Div e1 e2) = T.FunCallE T.Div [desugar e1, desugar e2] ()
  desugar (P.BinOp _ P.Mod e1 e2) = T.FunCallE T.Mod [desugar e1, desugar e2] ()

  desugar (P.BinOp _ P.Eq  e1 e2) = T.FunCallE T.Eq  [desugar e1, desugar e2] ()
  desugar (P.BinOp _ P.Neq e1 e2) = T.FunCallE T.Neq [desugar e1, desugar e2] ()
  desugar (P.BinOp _ P.Lt  e1 e2) = T.FunCallE T.Lt  [desugar e1, desugar e2] ()
  desugar (P.BinOp _ P.Gt  e1 e2) = T.FunCallE T.Gt  [desugar e1, desugar e2] ()
  desugar (P.BinOp _ P.Lte e1 e2) = T.FunCallE T.Lte [desugar e1, desugar e2] ()
  desugar (P.BinOp _ P.Gte e1 e2) = T.FunCallE T.Gte [desugar e1, desugar e2] ()

  desugar (P.BinOp _ P.And e1 e2) = T.FunCallE T.And [desugar e1, desugar e2] ()
  desugar (P.BinOp _ P.Or e1 e2)  = T.FunCallE T.Or  [desugar e1, desugar e2] ()
  desugar (P.BinOp _ P.Cons e1 e2) = T.FunCallE T.Cons [desugar e1, desugar e2] ()


instance Desugar P.ExprLookup (T.Expr ()) where
  desugar :: P.ExprLookup -> T.Expr ()
  desugar (P.ExprField expr P.Head) = T.FunCallE T.HeadFun [desugar expr] ()
  desugar (P.ExprField expr P.Tail) = T.FunCallE T.TailFun [desugar expr] ()
  desugar (P.ExprField expr P.Fst)  = T.FunCallE T.FstFun  [desugar expr] ()
  desugar (P.ExprField expr P.Snd)  = T.FunCallE T.SndFun  [desugar expr] ()


instance Desugar P.Type U.UType where
  desugar :: P.Type -> U.UType
  desugar (P.IntT  _) = U.Int
  desugar (P.BoolT _) = U.Bool
  desugar (P.CharT _) = U.Char
  desugar (P.Prod _ t1 t2) = U.Prod (desugar t1) (desugar t2)
  desugar (P.List _ t) = U.List (desugar t)
  desugar (P.Void _) = U.Void
  desugar (P.Fun _ ts t) = U.Fun (desugar <$> ts) (desugar t)
  -- TODO: We probably want to replace named type variables with
  -- de Bruijn indices here
  desugar (P.TyVar _ name) = U.TVar name
  desugar P.GarbageT = error "Attempted to desugar GarbageT node from ParseAST!"
