module Syntax.TypeAST (
  Program(..),
  VarDecl(..),
  FunDecl(..),
  Stmt(..),
  Type(..),
  VarLookup(..),
  Field(..),
  Expr(..)
) where
import qualified Data.Text as T

data Program a = Program [VarDecl a] [FunDecl a]
  deriving Show

data VarDecl a = VarDecl (Maybe Type) T.Text (Expr a)
  deriving Show

data FunDecl a = FunDecl T.Text [T.Text] (Maybe Type) [VarDecl a] [Stmt a]
  deriving Show

data Stmt a = If (Expr a) [Stmt a] [Stmt a]
            | While (Expr a) [Stmt a]
            | Assign VarLookup (Expr a)
            | FunCall FunName [Expr a]
            | Return (Maybe (Expr a))
  deriving Show

data Type = IntT
          | BoolT
          | CharT
          | Prod Type Type
          | List Type
          | Void
          | Fun [Type] Type
          | TyVar T.Text
  deriving (Show, Eq)

data VarLookup = VarId T.Text | VarField VarLookup Field
  deriving Show

data Field = Head | Tail | Fst | Snd
  deriving Show

data FunName = FunName T.Text
             | Not | Neg
             | Add | Sub | Mul | Div | Mod
             | Eq | Neq | Lt | Gt | Lteq | Gteq
             | And | Or
             | Cons | IsEmpty | Print
             | HeadFun | TailFun | FstFun | SndFun
  deriving Show

data Expr a = Ident T.Text a
            | Int Integer a
            | Char Char a
            | Bool Bool a
            | FunCallE FunName [Expr a] a
            | EmptyList a
            | Tuple (Expr a) (Expr a) a
  deriving Show
