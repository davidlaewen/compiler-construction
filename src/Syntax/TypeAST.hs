module Syntax.TypeAST (
  Program(..),
  VarDecl(..),
  FunDecl(..),
  Stmt(..),
  VarLookup(..),
  Field(..),
  FunName(..),
  Expr(..)
) where

import qualified Data.Text as T
import TypeInference.Definition

data Program varAnnot funAnnot = Program [VarDecl varAnnot] [FunDecl varAnnot funAnnot]
  deriving Show

data VarDecl a = VarDecl (Maybe UType) T.Text (Expr a) a
  deriving Show

data FunDecl a b = FunDecl T.Text [T.Text] (Maybe UType) [VarDecl a] [Stmt a] b
  deriving Show

data Stmt a = If (Expr a) [Stmt a] [Stmt a]
            | While (Expr a) [Stmt a]
            | Assign VarLookup (Expr a)
            | FunCall FunName [Expr a]
            | Return (Maybe (Expr a))
  deriving Show

data VarLookup = VarId T.Text | VarField VarLookup Field
  deriving Show

data Field = Head | Tail | Fst | Snd
  deriving Show

data FunName = Name T.Text
             | Not | Neg
             | Add | Sub | Mul | Div | Mod
             | Eq | Neq | Lt | Gt | Lte | Gte
             | And | Or
             | Cons | IsEmpty | Print
             | HeadFun | TailFun | FstFun | SndFun
  deriving Show

data Expr a = Ident T.Text a
            | Int Int a
            | Char Char a
            | Bool Bool a
            | FunCallE FunName [Expr a] a
            | EmptyList a
            | Tuple (Expr a) (Expr a) a
  deriving Show
