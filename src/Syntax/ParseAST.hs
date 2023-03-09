module Syntax.ParseAST (
  Program(..),
  VarDecl(..),
  FunDecl(..),
  Stmt(..),
  Type(..),
  VarLookup(..),
  ExprLookup(..),
  Field(..),
  Expr(..),
  UnaryOp(..),
  BinaryOp(..)
) where

import qualified Data.Text as T

data Program = Program [VarDecl] [FunDecl]
  deriving Show

data VarDecl = VarDecl (Maybe Type) T.Text Expr
  deriving Show

data FunDecl = FunDecl T.Text [T.Text] (Maybe Type) [VarDecl] [Stmt]
  deriving Show

data Stmt = If Expr [Stmt] [Stmt]
            | While Expr [Stmt]
            | Assign VarLookup Expr
            | FunCall T.Text [Expr]
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
          | TyVar T.Text
          | GarbageT
  deriving (Show, Eq)

data VarLookup = VarId T.Text | VarField VarLookup Field
  deriving Show

data ExprLookup = ExprField Expr Field
  deriving Show

data Field = Head | Tail | Fst | Snd
  deriving Show

data Expr = Ident T.Text
          | ExprLookup ExprLookup
          | Int Integer
          | Char Char
          | Bool Bool
          | UnOp UnaryOp Expr
          | BinOp BinaryOp Expr Expr
          | FunCallE T.Text [Expr]
          | EmptyList
          | Tuple Expr Expr
  deriving Show

data UnaryOp = Not | Neg
  deriving Show

data BinaryOp = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Lteq | Gteq | And | Or | Cons
  deriving Show
