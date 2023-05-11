{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module CodeGen.CodeGen
  ( codegen,
    runCodegen,
    Program,
  )
where

import qualified Data.Text as T
import Syntax.TypeAST (FunDecl (FunDecl), Expr(..), Stmt (..), VarDecl (..))
import qualified Syntax.TypeAST as TypeAST
import TypeInference.Definition
import Control.Monad.State (State, evalState, modify, gets)
import qualified TypeInference.Definition as UType
import qualified Data.Map as M

data CodegenState = CodegenState { labelCounter :: Int, localOffsets :: M.Map T.Text Int }
type Codegen = State CodegenState

data Register = PC | SP | MP | HP | RR
  deriving (Show)

data Instr = Label T.Text | Ret | Str Register | Ldc Integer | Bsr T.Text | Bra T.Text | Brf T.Text | Halt | Add | Link Int | Unlink | Ldl Int | Stl Int
  deriving (Show)

type Program = [Instr]

freshLabel :: Codegen T.Text
freshLabel = do
  i <- gets labelCounter
  modify (\s -> s { labelCounter = i + 1 })
  pure $ "label" <> T.pack (show i)

modifyLocalOffsets :: (M.Map T.Text Int -> M.Map T.Text Int) -> Codegen ()
modifyLocalOffsets f = modify (\s -> s { localOffsets = f (localOffsets s) })

lookupLocalOffset :: T.Text -> Codegen (Maybe Int)
lookupLocalOffset ident = gets (M.lookup ident . localOffsets)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f [] = pure []
concatMapM f (x : xs) = do
  y <- f x
  ys <- concatMapM f xs
  pure $ y ++ ys

runCodegen :: Codegen a -> a
runCodegen = flip evalState initialState
  where
    initialState = CodegenState 0 M.empty

codegen :: TypeAST.Program UType UScheme -> Codegen Program
codegen (TypeAST.Program [] funDecls) = do
  program <- concatMapM codegenFunDecl funDecls
  pure $ Bsr "main" : Halt : program
codegen (TypeAST.Program _ _) = error "TODO: vardecls"

codegenFunDecl :: FunDecl UType UScheme -> Codegen Program
codegenFunDecl (FunDecl funName [] _ varDecls stmts uscheme) = do
  modifyLocalOffsets (const M.empty)
  let varDeclsDize = foldr (\(VarDecl _ _ _ ty) -> (+ uTypeSize ty)) 0 varDecls
  varDeclsProgram <- concatMapM codegenVarDecl (zip [0..] varDecls)
  stmtsProgram <- concatMapM codegenStmt stmts
  pure $ Label funName : Link varDeclsDize : varDeclsProgram ++ stmtsProgram
codegenFunDecl (FunDecl funName argNames _ varDecls stmts uscheme) = error "TODO: vardecls"

codegenVarDecl :: (Int, VarDecl UType) -> Codegen Program
codegenVarDecl (i, VarDecl _ ident e _) = do
  program <- codegenExpr e
  modifyLocalOffsets (M.insert ident i)
  pure $ program ++ [Stl i]

uTypeSize :: UType -> Int
uTypeSize UType.Int = 1
uTypeSize UType.Bool = 1
uTypeSize UType.Char = 1
uTypeSize (UType.Prod t1 t2) = uTypeSize t1 + uTypeSize t2
uTypeSize (UType.List _) = 1
uTypeSize t = error $ "Called uTypeSize on illegal type: " <> show t

codegenStmt :: Stmt UType -> Codegen Program
codegenStmt (Return Nothing) = pure [Unlink, Ret]
codegenStmt (Return (Just expr)) = do
  program <- codegenExpr expr
  pure $ program ++ [Str RR, Unlink, Ret]
codegenStmt (If condition thenStmts elseStmts) = do
  conditionProgram <- codegenExpr condition
  thenProgram <- concatMapM codegenStmt thenStmts
  elseProgram <- concatMapM codegenStmt elseStmts
  elseLabel <- freshLabel
  endLabel <- freshLabel
  pure $ conditionProgram ++ [Brf elseLabel] ++ thenProgram ++ [Bra endLabel, Label elseLabel] ++ elseProgram ++ [Label endLabel]
codegenStmt stmt = error $ "TODO: codegenStmt: " <> show stmt

codegenExpr :: Expr UType -> Codegen Program
codegenExpr (TypeAST.Int i _) = pure [Ldc i]
codegenExpr (TypeAST.Bool True _) = pure [Ldc (-1)]
codegenExpr (TypeAST.Bool False _) = pure [Ldc 0]
codegenExpr (TypeAST.Ident ident _) = do
  lookupLocalOffset ident >>= \case
    Nothing -> error $ "Tried to lookup local `" <> T.unpack ident <> "` but it wasn't in the map"
    Just offset -> pure [Ldl offset]
codegenExpr (TypeAST.FunCallE TypeAST.Add [e1, e2] _) = do
  program1 <- codegenExpr e1
  program2 <- codegenExpr e2
  pure $ program1 ++ program2 ++ [Add]
codegenExpr expr = error $ "TODO: codegenExpr: " <> show expr
