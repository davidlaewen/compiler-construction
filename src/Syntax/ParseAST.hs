{-# LANGUAGE InstanceSigs #-}

module Syntax.ParseAST (
  Program(..),
  DataDecl(..),
  DataConstr(..),
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
import Utils.Loc ( Loc(..), HasLoc(..), defaultLoc )
import Parser.Tokens (Symbol(..))


data Program = Program [DataDecl] [VarDecl] [FunDecl]
  deriving Show

data DataDecl = DataDecl Loc T.Text [DataConstr]
  deriving Show

data DataConstr = DataConstr Loc T.Text [(T.Text, Type)]
  deriving Show

data VarDecl = VarDecl Loc (Maybe Type) T.Text Expr
  deriving Show

data FunDecl = FunDecl Loc T.Text [T.Text] (Maybe Type) [VarDecl] [Stmt]
  deriving Show

data Stmt = If Loc Expr [Stmt] [Stmt]
          | While Loc Expr [Stmt]
          | Assign Loc VarLookup Expr
          | FunCall Loc T.Text [Expr]
          | Return Loc (Maybe Expr)
          | GarbageStmt
  deriving Show

data Type = IntT Loc
          | BoolT Loc
          | CharT Loc
          | Prod Loc Type Type
          | List Loc Type
          | Void Loc
          | Fun Loc [Type] Type
          | DataT Loc T.Text
          | TyVar Loc T.Text
          | GarbageType
  deriving (Show, Eq)

data VarLookup = VarId Loc T.Text | VarField Loc VarLookup Field
  deriving Show

data ExprLookup = ExprField Expr Field
  deriving Show

data Field = Head | Tail | Fst | Snd | Selector T.Text | GarbageField
  deriving Show

data Expr = Ident Loc T.Text
          | ExprLookup Loc ExprLookup
          | Int Loc Int
          | Bool Loc Bool
          | Char Loc Char
          | UnOp Loc UnaryOp Expr
          | BinOp Loc BinaryOp Expr Expr
          | FunCallE Loc T.Text [Expr]
          | ConstrCall Loc T.Text [Expr]
          | EmptyList Loc
          | Tuple Loc Expr Expr
          | GarbageExpr
  deriving Show

data UnaryOp = Not | Neg
  deriving (Eq,Ord)

instance Show UnaryOp where
  show Not = show SymBang
  show Neg = show SymMinus

data BinaryOp = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Lte | Gte | And | Or | Cons
  deriving (Eq,Ord)

instance Show BinaryOp where
  show Add = show SymPlus
  show Sub = show SymMinus
  show Mul = show SymAst
  show Div = show SymSlash
  show Mod = show SymPercent
  show Eq = show SymEqEq
  show Neq = show SymBangEq
  show Lt = show SymLessThan
  show Gt = show SymGreaterThan
  show Lte = show SymLessThanEq
  show Gte = show SymGreaterThanEq
  show And = show SymAndAnd
  show Or = show SymPipePipe
  show Cons = show SymColon

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
  getLoc GarbageStmt = defaultLoc

instance HasLoc Type where
  getLoc :: Type -> Loc
  getLoc (IntT loc) = loc
  getLoc (BoolT loc) = loc
  getLoc (CharT loc) = loc
  getLoc (Prod loc _ _) = loc
  getLoc (List loc _) = loc
  getLoc (Void loc) = loc
  getLoc (Fun loc _ _) = loc
  getLoc (DataT loc _) = loc
  getLoc (TyVar loc _) = loc
  getLoc GarbageType = defaultLoc

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
  getLoc (ConstrCall loc _ _) = loc
  getLoc (FunCallE loc _ _) = loc
  getLoc (EmptyList loc) = loc
  getLoc (Tuple loc _ _) = loc
  getLoc GarbageExpr = defaultLoc
