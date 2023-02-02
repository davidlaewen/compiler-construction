module Syntax.Terms (
  Program(..),
  VarDecl(..),
  FunDecl(..),
  Id,
  Stmt(..),
  Field(..),
  Expr(..),
  UnaryOp(..),
  BinaryOp(..)
) where

import Syntax.Common (Id)
import qualified Syntax.Types as Ty

data Program = Program [VarDecl] [FunDecl]

data VarDecl = VarDecl (Maybe Ty.Type) Id Expr
data FunDecl = FunDecl Id [Id] (Maybe Ty.Type) [VarDecl] [Stmt]

data Stmt = If Expr [Stmt] [Stmt]
          | While Expr [Stmt]
          | Assign Field Expr
          | FunCall Id [Expr]
          | Return (Maybe Expr)

data Field = Ident Id
           | Head Field
           | Tail Field
           | Fst Field
           | Snd Field

data Expr = Field
          | Num Int
          | Char Char
          | True | False
          | UnOp UnaryOp Expr
          | BinOp BinaryOp Expr Expr
          | FunCallE Id [Expr]
          | EmptyList
          | Tuple Expr Expr

data UnaryOp = Not | Neg

data BinaryOp = Add | Sub | Mul | Div | Mod | Eq | Le | Ge | Leq | Geq | Neq | And | Or | Cons
