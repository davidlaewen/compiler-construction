module Syntax.TypeAST (
  Program(..),
  VarDecl(..),
  FunDecl(..),
  FunMutDecl(..),
  Stmt(..),
  VarLookup(..),
  Field(..),
  FunName(..),
  Expr(..),
  getTypeExpr
) where

import qualified Data.Text as T
import TypeInference.Types ( UType )
import Utils.Loc (Loc, HasLoc(..))

data Program varAnnot funAnnot = Program [VarDecl varAnnot] [FunMutDecl varAnnot funAnnot]
  deriving Show

data VarDecl a = VarDecl Loc (Maybe UType) T.Text (Expr a) a
  deriving Show

data FunDecl a b = FunDecl Loc T.Text [T.Text] (Maybe UType) [VarDecl a] [Stmt a] b
  deriving Show

data FunMutDecl a b = MutualDecls Loc [FunDecl a b] | SingleDecl (FunDecl a b)
  deriving Show

data Stmt a = If Loc (Expr a) [Stmt a] [Stmt a]
            | While Loc (Expr a) [Stmt a]
            -- | We need type info in assignments during code gen
            | Assign Loc VarLookup a (Expr a)
            | FunCall Loc FunName [Expr a]
            | Return Loc (Maybe (Expr a))
  deriving Show

data VarLookup = VarId Loc T.Text | VarField Loc VarLookup Field
  deriving Show

data Field = Head | Tail | Fst | Snd
  deriving Show

data FunName = Name T.Text
             | Not | Neg
             | Add | Sub | Mul | Div | Mod
             | Eq | Neq | Lt | Gt | Lte | Gte
             | And | Or
             | Cons | IsEmpty
             | HeadFun | TailFun | FstFun | SndFun
             | Print

data Expr a = Ident Loc T.Text a
            | Int Loc Int a
            | Char Loc Char a
            | Bool Loc Bool a
            | FunCallE Loc FunName [Expr a] a
            | EmptyList Loc a
            | Tuple Loc (Expr a) (Expr a) a
  deriving Show

getTypeExpr :: Expr a -> a
getTypeExpr (Ident _ _ ty) = ty
getTypeExpr (Int _ _ ty) = ty
getTypeExpr (Char _ _ ty) = ty
getTypeExpr (Bool _ _ ty) = ty
getTypeExpr (FunCallE _ _ _ ty) = ty
getTypeExpr (EmptyList _ ty) = ty
getTypeExpr (Tuple _ _ _ ty) = ty

------------------------------------

instance Show FunName where
    show (Name t) = T.unpack t
    show Not = "_not"
    show Neg = "_neg"
    show Add = "_add"
    show Sub = "_sub"
    show Mul = "_mul"
    show Div = "_div"
    show Mod = "_mod"
    show Eq = "_eq"
    show Neq = "_neq"
    show Lt = "_lt"
    show Gt = "_gt"
    show Lte = "_lte"
    show Gte = "_gte"
    show And = "_and"
    show Or = "_or"
    show Cons = "_cons"
    show IsEmpty = "_isEmpty"
    show HeadFun = "_head"
    show TailFun = "_tail"
    show FstFun = "_fst"
    show SndFun = "_snd"
    show Print = "_print"


-------------------------------------
-- HasLoc instance declarations

instance HasLoc (VarDecl a) where
  getLoc (VarDecl loc _ _ _ _) = loc

instance HasLoc (FunDecl a b) where
  getLoc (FunDecl loc _ _ _ _ _ _) = loc

instance HasLoc (Stmt a) where
  getLoc (If loc _ _ _) = loc
  getLoc (While loc _ _) = loc
  getLoc (Assign loc _ _ _) = loc
  getLoc (FunCall loc _ _) = loc
  getLoc (Return loc _) = loc

instance HasLoc VarLookup where
  getLoc (VarId loc _) = loc
  getLoc (VarField loc _ _) = loc

instance HasLoc (Expr a) where
  getLoc (Ident loc _ _) = loc
  getLoc (Int loc _ _) = loc
  getLoc (Char loc _ _) = loc
  getLoc (Bool loc _ _) = loc
  getLoc (FunCallE loc _ _ _) = loc
  getLoc (EmptyList loc _) = loc
  getLoc (Tuple loc _ _ _) = loc
