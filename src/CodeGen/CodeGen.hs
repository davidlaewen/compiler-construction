{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module CodeGen.CodeGen(
  codegen,
  runCodegen,
  Program,
) where

import qualified Data.Text as T
import CodeGen.Definition
import CodeGen.Instructions (Register(..), Instr(..))
import Syntax.TypeAST (FunDecl(..), Expr(..), Stmt(..), VarDecl(..), FunName(..), VarLookup(..))
import qualified Syntax.TypeAST as TypeAST
import TypeInference.Definition (UScheme, UType)
import Control.Monad.State (gets, forM_)
import qualified Data.Map as M


lookupOffset :: T.Text -> Codegen Loc
lookupOffset ident = gets (M.lookup ident . offsets) >>= \case
  Nothing -> gets (M.lookup ident . heapLocs) >>= \case
    Nothing -> error $ "Couldn't find offset for identifer " <> T.unpack ident
    Just offset -> pure $ HeapLoc offset
  Just offset -> pure $ Offset offset

loadIdent :: T.Text -> Codegen Program
loadIdent ident = lookupOffset ident >>= \case
  Offset offset -> pure [LoadLocal offset]
  HeapLoc offset -> pure [LoadConst (heapLow + offset), LoadHeap 0]

storeIdent :: T.Text -> Codegen Program
storeIdent ident = lookupOffset ident >>= \case
  Offset offset -> pure [StoreLocal offset]
  HeapLoc offset -> pure [LoadConst (heapLow + offset), StoreAddress 0]

codegen :: TypeAST.Program UType UScheme -> Codegen Program
codegen (TypeAST.Program varDecls funDecls) = do
  -- TODO: Global variable declarations
  let varDeclsSize = sum $ map (\(VarDecl _ _ _ ty) -> uTypeSize ty) varDecls
  varDeclsProgram <- concatMapM codegenGlobalVarDecl (zip [0..] varDecls)
  funDeclsProgram <- concatMapM codegenFunDecl funDecls
  pure $
    varDeclsProgram ++ Adjust (negate varDeclsSize) :
      BranchAlways "main" : funDeclsProgram ++ [Halt]

codegenGlobalVarDecl :: (Int, VarDecl UType) -> Codegen Program
codegenGlobalVarDecl (i, VarDecl _ ident e _) = do
  program <- codegenExpr e
  modifyHeapLocs (M.insert ident i)
  -- Store immediately, since later global vars may use this declaration
  pure $ program ++ [StoreHeap]

codegenLocalVarDecl :: (Int, VarDecl UType) -> Codegen Program
codegenLocalVarDecl (i, VarDecl _ ident e _) = do
  program <- codegenExpr e
  modifyOffsets (M.insert ident i)
  pure $ program ++ [StoreLocal i]

codegenFunDecl :: FunDecl UType UScheme -> Codegen Program
codegenFunDecl (FunDecl funName args retType varDecls stmts uScheme) = do
  modifyOffsets (const M.empty)
  -- Arguments at negative offsets from MP, reverse order
  -- TODO: Calculate sizes
  forM_ (zip (reverse args) (map (* (-1)) [2..]))
    (\(argName, i) -> modifyOffsets (M.insert argName i))
  let varDeclsSize = sum $ map (\(VarDecl _ _ _ ty) -> uTypeSize ty) varDecls
  varDeclsProgram <- concatMapM codegenLocalVarDecl (zip [1..] varDecls)
  stmtsProgram <- concatMapM codegenStmt stmts
  pure $ Label funName : Link varDeclsSize : varDeclsProgram ++ stmtsProgram

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
  storeProgram <- storeIdent ident
  exprProgram <- codegenExpr expr
  pure $ exprProgram ++ storeProgram

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

codegenStmt stmt = error $ "Case of codegenStmt not yet implemented: " <> show stmt

codegenExpr :: Expr UType -> Codegen Program
codegenExpr (TypeAST.Int i _) = pure [LoadConst i]
codegenExpr (TypeAST.Bool True _) = pure [LoadConst (-1)]
codegenExpr (TypeAST.Bool False _) = pure [LoadConst 0]
codegenExpr (TypeAST.Ident ident _) = loadIdent ident

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
