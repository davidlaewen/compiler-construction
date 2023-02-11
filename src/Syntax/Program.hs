module Syntax.Program (
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
  deriving Show

data VarDecl = VarDecl (Maybe Ty.Type) Id Expr
  deriving Show

data FunDecl = FunDecl Id [Id] (Maybe Ty.Type) [VarDecl] [Stmt]
  deriving Show

data Stmt = If Expr [Stmt] [Stmt]
          | While Expr [Stmt]
          | Assign Field Expr
          | FunCall Id [Expr]
          | Return (Maybe Expr)
  deriving Show

data Field = Ident Id
           | Head Field
           | Tail Field
           | Fst Field
           | Snd Field
  deriving Show

data Expr = Field Field
          | Int Integer
          | Char Char
          | Bool Bool
          | UnOp UnaryOp Expr
          | BinOp BinaryOp Expr Expr
          | FunCallE Id [Expr]
          | EmptyList
          | Tuple Expr Expr
  deriving Show

data UnaryOp = Not | Neg
  deriving Show

data BinaryOp = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Lteq | Gteq | And | Or | Cons
  deriving Show
