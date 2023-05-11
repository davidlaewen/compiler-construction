module CodeGen.CodeGen
  ( codegen,
    Program,
  )
where

import qualified Data.Text as T
import Syntax.TypeAST (FunDecl (FunDecl), Stmt)
import qualified Syntax.TypeAST as TypeAST
import TypeInference.Definition

data Instr = Label T.Text | Ret | Ldc
  deriving (Show)

type Program = [Instr]

codegen :: TypeAST.Program UType UScheme -> Program
codegen (TypeAST.Program [] funDecls) = foldMap codegenFunDecl funDecls
codegen (TypeAST.Program _ _) = error "TODO: vardecls"

codegenFunDecl :: FunDecl UType UScheme -> Program
codegenFunDecl (FunDecl funName argNames _ [] stmts uscheme) = Label funName : foldMap codegenStmt stmts ++ [Ret]
codegenFunDecl (FunDecl funName argNames _ varDecls stmts uscheme) = error "TODO: vardecls"

codegenStmt :: Stmt UType -> Program
codegenStmt stmt = error $ "TODO: codegenStmt: " <> show stmt
