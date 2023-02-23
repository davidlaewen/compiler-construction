module Syntax.Program (
  Program(..),
  VarDecl(..),
  FunDecl(..),
  Id,
  Stmt(..),
  Type(..),
  Field(..),
  Expr(..),
  UnaryOp(..),
  BinaryOp(..)
) where

import Syntax.Common (Id)

data Program = Program [VarDecl] [FunDecl]
  deriving Show

data VarDecl = VarDecl (Maybe Type) Id Expr
  deriving Show

data FunDecl = FunDecl Id [Id] (Maybe Type) [VarDecl] [Stmt]
  deriving Show

data Stmt = If Expr [Stmt] [Stmt]
          | While Expr [Stmt]
          | Assign Field Expr
          | FunCall Id [Expr]
          | Return (Maybe Expr)
          | GarbageS
  deriving Show

data Type = IntT
          | BoolT
          | CharT
          | Prod Type Type
          | List Type
          | Void
          | Fun [Type] Type
          | TyVar Id
          | GarbageT
  deriving (Show, Eq)

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
