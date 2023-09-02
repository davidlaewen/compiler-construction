{-# LANGUAGE InstanceSigs #-}

module Syntax.ParseAST (
  Program(..),
  VarDecl(..),
  FunDecl(..),
  FunMutDecl(..),
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
import Utils.Loc ( Loc(..), HasLoc(..), defaultLoc )


data Program = Program [VarDecl] [FunMutDecl]
  deriving Show

data VarDecl = VarDecl Loc (Maybe Type) T.Text Expr
  deriving Show

data FunDecl = FunDecl Loc T.Text [T.Text] (Maybe Type) [VarDecl] [Stmt]
  deriving Show

data FunMutDecl = MutualDecls Loc [FunDecl] | SingleDecl FunDecl
  deriving Show

data Stmt = If Loc Expr [Stmt] [Stmt]
          | While Loc Expr [Stmt]
          | Assign Loc VarLookup Expr
          | FunCall Loc T.Text [Expr]
          | Return Loc (Maybe Expr)
          | GarbageS
  deriving Show

data Type = IntT Loc
          | BoolT Loc
          | CharT Loc
          | Prod Loc Type Type
          | List Loc Type
          | Void Loc
          | Fun Loc [Type] Type
          | TyVar Loc T.Text
          | GarbageT
  deriving (Show, Eq)

data VarLookup = VarId Loc T.Text | VarField Loc VarLookup Field
  deriving Show

data ExprLookup = ExprField Expr Field
  deriving Show

data Field = Head | Tail | Fst | Snd
  deriving Show

data Expr = Ident Loc T.Text
          | ExprLookup Loc ExprLookup
          | Int Loc Int
          | Bool Loc Bool
          | Char Loc Char
          | UnOp Loc UnaryOp Expr
          | BinOp Loc BinaryOp Expr Expr
          | FunCallE Loc T.Text [Expr]
          | EmptyList Loc
          | Tuple Loc Expr Expr
  deriving Show

data UnaryOp = Not | Neg
  deriving Show

data BinaryOp = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Lte | Gte | And | Or | Cons
  deriving Show


-------------------------------------------
-- HasLoc instance declarations

instance HasLoc VarDecl where
  getLoc :: VarDecl -> Loc
  getLoc (VarDecl loc _ _ _) = loc

instance HasLoc FunDecl where
  getLoc :: FunDecl -> Loc
  getLoc (FunDecl loc _ _ _ _ _) = loc

instance HasLoc Stmt where
  getLoc :: Stmt -> Loc
  getLoc (If loc _ _ _) = loc
  getLoc (While loc _ _) = loc
  getLoc (Assign loc _ _) = loc
  getLoc (FunCall loc _ _) = loc
  getLoc (Return loc _) = loc
  getLoc GarbageS = defaultLoc

instance HasLoc Type where
  getLoc :: Type -> Loc
  getLoc (IntT loc) = loc
  getLoc (BoolT loc) = loc
  getLoc (CharT loc) = loc
  getLoc (Prod loc _ _) = loc
  getLoc (List loc _) = loc
  getLoc (Void loc) = loc
  getLoc (Fun loc _ _) = loc
  getLoc (TyVar loc _) = loc
  getLoc GarbageT = defaultLoc

instance HasLoc VarLookup where
  getLoc :: VarLookup -> Loc
  getLoc (VarId loc _) = loc
  getLoc (VarField loc _ _) = loc

instance HasLoc Expr where
  getLoc :: Expr -> Loc
  getLoc (Ident loc _) = loc
  getLoc (ExprLookup loc _ ) = loc
  getLoc (Int loc _) = loc
  getLoc (Bool loc _) = loc
  getLoc (Char loc _) = loc
  getLoc (UnOp loc _ _) = loc
  getLoc (BinOp loc _ _ _) = loc
  getLoc (FunCallE loc _ _) = loc
  getLoc (EmptyList loc) = loc
  getLoc (Tuple loc _ _) = loc
