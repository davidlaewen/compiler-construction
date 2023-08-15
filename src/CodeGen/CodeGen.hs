{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module CodeGen.CodeGen
  ( codegen,
    runCodegen,
    Program,
  )
where

import qualified Data.Text as T
import CodeGen.Instructions (Register(..), Instr(..))
import Syntax.TypeAST (FunDecl(..), Expr(..), Stmt(..), VarDecl(..), FunName(..), VarLookup(..))
import qualified Syntax.TypeAST as TypeAST
import TypeInference.Definition (UScheme, UType)
import Control.Monad.State (State, evalState, modify, gets, forM_)
import qualified TypeInference.Definition as UType
import qualified Data.Map as M

data CodegenState = CodegenState { labelCounter :: Int, localOffsets :: M.Map T.Text Int }
type Codegen = State CodegenState

type Program = [Instr]

freshLabel :: T.Text -> Codegen T.Text
freshLabel t = do
  i <- gets labelCounter
  modify (\s -> s { labelCounter = i + 1 })
  pure $ t <> "_" <> T.pack (show i)

modifyLocalOffsets :: (M.Map T.Text Int -> M.Map T.Text Int) -> Codegen ()
modifyLocalOffsets f = modify (\s -> s { localOffsets = f (localOffsets s) })

lookupLocalOffset :: T.Text -> Codegen Int
lookupLocalOffset ident = gets (M.lookup ident . localOffsets) >>= \case
  Nothing -> error $ "Couldn't find entry for local identifier " <> T.unpack ident
  Just offset -> pure offset

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM _ [] = pure []
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
  pure $ BranchAlways "main" : program ++ [Halt]
codegen (TypeAST.Program _ _) = error "TODO: vardecls"

codegenFunDecl :: FunDecl UType UScheme -> Codegen Program
codegenFunDecl (FunDecl funName args retType varDecls stmts uScheme) = do
  modifyLocalOffsets (const M.empty)
  -- Arguments at negative offsets from MP, reverse order
  -- TODO: Calculate sizes
  forM_ (zip (reverse args) (map (* (-1)) [2..]))
    (\(argName, i) -> modifyLocalOffsets (M.insert argName i))
  let varDeclsSize = foldr (\(VarDecl _ _ _ ty) -> (+ uTypeSize ty)) 0 varDecls
  varDeclsProgram <- concatMapM codegenVarDecl (zip [1..] varDecls)
  stmtsProgram <- concatMapM codegenStmt stmts
  pure $ Label funName : Link varDeclsSize : varDeclsProgram ++ stmtsProgram

codegenVarDecl :: (Int, VarDecl UType) -> Codegen Program
codegenVarDecl (i, VarDecl _ ident e _) = do
  program <- codegenExpr e
  modifyLocalOffsets (M.insert ident i)
  pure $ program ++ [StoreLocal i]

uTypeSize :: UType -> Int
uTypeSize UType.Int = 1
uTypeSize UType.Bool = 1
uTypeSize UType.Char = 1
uTypeSize (UType.Prod t1 t2) = uTypeSize t1 + uTypeSize t2
uTypeSize (UType.List _) = 1
uTypeSize t = error $ "Called uTypeSize on illegal type: " <> show t

codegenStmt :: Stmt UType -> Codegen Program
codegenStmt (If condition thenStmts elseStmts) = do
  conditionProgram <- codegenExpr condition
  thenProgram <- concatMapM codegenStmt thenStmts
  elseProgram <- concatMapM codegenStmt elseStmts
  elseLabel <- freshLabel "else"
  endLabel <- freshLabel "endif"
  pure $
    conditionProgram ++ [BranchFalse elseLabel]
    ++ thenProgram ++ [BranchAlways endLabel, Label elseLabel]
    ++ elseProgram ++ [Label endLabel]

codegenStmt (While cond loopStmts) = do
  condProgram <- codegenExpr cond
  loopProgram <- concatMapM codegenStmt loopStmts
  topLabel <- freshLabel "while"
  endLabel <- freshLabel "endwhile"
  pure $
    [Label topLabel] ++ condProgram ++ [BranchFalse endLabel]
    ++ loopProgram ++ [BranchAlways topLabel]
    ++ [Label endLabel]

codegenStmt (Assign (VarId ident) expr) = do
  offset <- lookupLocalOffset ident
  exprProgram <- codegenExpr expr
  pure $ exprProgram ++ [StoreLocal offset]

codegenStmt (FunCall funName args) = do
  argsProgram <- concatMapM codegenExpr args
  pure $ argsProgram ++
    case funName of
      -- TODO: Calculate sizes
      Name name -> [BranchSubr name, Adjust $ (-1) * length args]
      _ -> [funName2Instr funName]

codegenStmt (Return Nothing) = pure [Unlink, Ret]
codegenStmt (Return (Just expr)) = do
  program <- codegenExpr expr
  pure $ program ++ [StoreReg RetReg, Unlink, Ret]

codegenStmt stmt = error $ "TODO: codegenStmt: " <> show stmt

codegenExpr :: Expr UType -> Codegen Program
codegenExpr (TypeAST.Int i _) = pure [LoadConst i]
codegenExpr (TypeAST.Bool True _) = pure [LoadConst (-1)]
codegenExpr (TypeAST.Bool False _) = pure [LoadConst 0]
codegenExpr (TypeAST.Ident ident _) = do
  offset <- lookupLocalOffset ident
  pure [LoadLocal offset]

codegenExpr (FunCallE funName args _) = do
  argsProgram <- concatMapM codegenExpr args
  pure $ argsProgram ++
    case funName of
      -- TODO: Calculate sizes
      Name name -> BranchSubr name : [Adjust $ (-1) * length args, LoadReg RetReg] -- Subroutine
      _ -> [funName2Instr funName] -- Primitive operation

codegenExpr expr = error $ "TODO: codegenExpr: " <> show expr


funName2Instr :: FunName -> Instr
funName2Instr (Name name) =
  error $ "funName2Instr was called with Name " <> T.unpack name
funName2Instr Not = NotOp
funName2Instr Neg = NegOp
funName2Instr Add = AddOp
funName2Instr Sub = SubOp
funName2Instr Mul = MulOp
funName2Instr Div = DivOp
funName2Instr Mod = ModOp
funName2Instr Eq = EqOp
funName2Instr Neq = NeOp
funName2Instr Lt = LtOp
funName2Instr Gt = GtOp
funName2Instr Lte = LtOp
funName2Instr Gte = GtOp
funName2Instr And = AndOp
funName2Instr Or = OrOp
funName2Instr Print = TrapInt
funName2Instr name = error $ "FunName " <> show name <> "not yet implemented"
