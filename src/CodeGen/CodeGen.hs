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
import qualified TypeInference.Definition as TI
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
  exprProgram <- codegenExpr expr
  storeProgram <- storeIdent ident
  pure $ exprProgram ++ storeProgram

codegenStmt (Assign (VarField varLookup field) expr) =
  case field of
    TypeAST.Head -> undefined
    TypeAST.Tail -> undefined
    TypeAST.Fst -> undefined
    TypeAST.Snd -> undefined

codegenStmt (FunCall funName args) = do
  argsProgram <- concatMapM codegenExpr args
  pure $ argsProgram ++
    case funName of
      -- TODO: Calculate sizes
      Name name -> [BranchSubr name, Adjust $ (-1) * length args]
      _ -> funName2Program funName (TypeAST.getTypeExpr <$> args)

codegenStmt (Return Nothing) = pure [Unlink, Ret]
codegenStmt (Return (Just expr)) = do
  program <- codegenExpr expr
  pure $ program ++ [StoreReg RetReg, Unlink, Ret]


codegenExpr :: Expr UType -> Codegen Program
codegenExpr (TypeAST.Ident ident _) = loadIdent ident
codegenExpr (TypeAST.Int i _) = pure [LoadConst i]
codegenExpr (TypeAST.Char c _) = pure [LoadConst $ fromEnum c]
codegenExpr (TypeAST.Bool True _) = pure [LoadConst (-1)]
codegenExpr (TypeAST.Bool False _) = pure [LoadConst 0]

codegenExpr (FunCallE funName args _) = do
  argsProgram <- concatMapM codegenExpr args
  pure $ argsProgram ++
    case funName of
      -- TODO: Calculate sizes
      Name name -> BranchSubr name : [Adjust $ (-1) * length args, LoadReg RetReg] -- Subroutine
      _ -> funName2Program funName (TypeAST.getTypeExpr <$> args) -- Primitive operation

codegenExpr (TypeAST.Tuple e1 e2 (TI.Prod ty1 ty2)) = do
  e1Program <- codegenExpr e1
  e2Program <- codegenExpr e2
  let size = uTypeSize ty1 + uTypeSize ty2
  pure $ e1Program ++ e2Program ++
    -- Push both entries to heap, compute address of left component
    [ StoreHeapMulti size, LoadConst (size - 1), SubOp ]

codegenExpr expr = error $ "TODO: codegenExpr: " <> show expr


funName2Program :: FunName -> [UType] -> Program
funName2Program (Name name) _ =
  error $ "funName2Instr was called with Name " <> T.unpack name
funName2Program Not _ = [NotOp]
funName2Program Neg _ = [NegOp]
funName2Program Add _ = [AddOp]
funName2Program Sub _ = [SubOp]
funName2Program Mul _ = [MulOp]
funName2Program Div _ = [DivOp]
funName2Program Mod _ = [ModOp]
funName2Program Eq  _ = [EqOp]
funName2Program Neq _ = [NeOp]
funName2Program Lt  _ = [LtOp]
funName2Program Gt  _ = [GtOp]
funName2Program Lte _ = [LtOp]
funName2Program Gte _ = [GtOp]
funName2Program And _ = [AndOp]
funName2Program Or  _ = [OrOp]
-- TODO: List operators
funName2Program Cons _ = undefined
funName2Program IsEmpty _ = undefined
funName2Program HeadFun _ = undefined
funName2Program TailFun _ = undefined
funName2Program FstFun [TI.Prod _ _] = [LoadHeap 0]
funName2Program FstFun _ = error "Called `fst` on non-tuple"
funName2Program SndFun [TI.Prod _ _] = [LoadHeap 1]
funName2Program SndFun _ = error "Called `snd` on non-tuple"

funName2Program Print [ty] = case ty of
  TI.Int -> [TrapInt]
  TI.Char -> [TrapChar]
  -- TI.Bool -> [BranchSubr "printBool", Adjust (-1)]
  _ -> error $ "Printing for type " <> show ty <> "not yet implemented!"

funName2Program Print _ = error "Function `print` called with multiple args!"
