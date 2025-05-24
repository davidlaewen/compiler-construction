{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module CodeGen.CodeGen(
  codegen,
  runCodegen,
  SSMProgram,
) where

import qualified Data.Text as T
import CodeGen.Definition
import CodeGen.Instructions (Register(..), Instr(..))
import Syntax.TypeAST
import TypeInference.Types (UScheme, UType)
import qualified TypeInference.Types as TI ( UScheme(..), UType(..) )
import Control.Monad.State (gets, forM_)
import qualified Data.Map as M

---------------------------------
-- Variable loading & storing

lookupLoc :: T.Text -> Codegen Location
lookupLoc ident = gets (M.lookup ident . offsets) >>= \case
  Nothing -> gets (M.lookup ident . heapLocs) >>= \case
    Nothing -> error $ "Couldn't find offset for identifer " <> T.unpack ident
    Just loc -> pure $ HeapLoc loc
  Just offset -> pure $ Offset offset

loadIdent :: T.Text -> UType -> Codegen SSMProgram
loadIdent ident _ =
  lookupLoc ident >>= \case
    Offset offset -> pure [LoadLocal offset]
    -- ldmh extends upwards, offset direction upwards
    HeapLoc loc -> pure [ LoadReg HeapLowReg, LoadHeap loc ]

loadWithOffset :: Int -> Instr
loadWithOffset = LoadHeap

computeOffsets :: [Int] -> Int -> [Int]
computeOffsets [] _ = []
computeOffsets (x:xs) acc = acc : computeOffsets xs (x + acc)


--------------------------
-- Code generation

codegen :: Program UType UScheme -> Codegen SSMProgram
codegen (Program dataDecls varDecls funDecls) = do
  forM_ dataDecls codegenDataDecl
  let varSizes = map (\(VarDecl _ _ _ _ ty) -> uTypeSize ty) varDecls
  -- TODO: All entries are single-register, simplify this
  let varOffsets = computeOffsets varSizes 0
  varDeclsProgram <- concatMapM codegenGlobalVarDecl (zip varOffsets varDecls)
  funDeclsProgram <- concatMapM codegenFunMutDecl funDecls
  pure $ varDeclsProgram ++
    -- HP now at start of global vars, copy value of HP to R5
    [LoadReg HeapPointer, StoreReg HeapLowReg] ++
    -- Store all global vars to heap, adjust SP
    StoreHeapMulti (sum varSizes) : -- Adjust (-1) :
      BranchSubr "main" : Halt : funDeclsProgram ++ [Halt]

codegenDataDecl :: DataDecl -> Codegen ()
codegenDataDecl (DataDecl _ _ ctors) = do
  -- Enumerate all constructors, starting at 0
  let ctorLabels = zip ctors [(0::Int)..]
  -- Map each ctor name to its label and field count
  forM_ ctorLabels codegenCtor

codegenCtor :: (Ctor,Int) -> Codegen ()
codegenCtor (Ctor _ cName args, cLabel) = do
  -- The offsets for n fields are -n, ..., -1 (in that order), since the stack
  -- entry points to the label, which is the last entry (offset 0).
  let fieldOffsets = zip (reverse (fst <$> args)) [(-1::Int),-2..]
  forM_ fieldOffsets codegenSelector
  insertCtorData cName $ CtorData cLabel (length args)
  where
    codegenSelector :: (T.Text,Int) -> Codegen ()
    codegenSelector (name,offset) = insertSelector name offset

codegenGlobalVarDecl :: (Int, VarDecl UType) -> Codegen SSMProgram
codegenGlobalVarDecl (i, VarDecl _ _ ident e _) = do
  program <- codegenExpr e
  modifyOffsets (M.insert ident i) -- Local offset for use in subsequent decls
  modifyHeapLocs (M.insert ident i)
  pure program
  -- pure $ program ++ [StoreHeapMulti $ uTypeSize ty]

codegenLocalVarDecl :: (Int, VarDecl UType) -> Codegen SSMProgram
codegenLocalVarDecl (i, VarDecl _ _ ident e _) = do
  program <- codegenExpr e
  modifyOffsets (M.insert ident i)
  -- Store immediately, since subsequent local vars may refer to this decl
  pure $ program ++ [StoreLocal i]

codegenFunMutDecl :: FunMutDecl UType UScheme -> Codegen SSMProgram
codegenFunMutDecl (SingleDecl funDecl) = codegenFunDecl funDecl
codegenFunMutDecl (MutualDecls _ funDecls) = concatMapM codegenFunDecl funDecls

codegenFunDecl :: FunDecl UType UScheme -> Codegen SSMProgram
codegenFunDecl (FunDecl _ funName args _ varDecls stmts uScheme) = do
  -- TODO: Polymorphic function types
  let argTypes = case uScheme of
        TI.UScheme _ (TI.Fun argTys _) -> argTys
        _ -> error ""
  modifyOffsets (const M.empty)
  let argSizes = 1 <$ argTypes
  -- Offsets should point to top. Negative offsets from MP, reverse order
  let argOffsets = succ <$> zipWith (-) (negate <$> computeOffsets argSizes 2) argSizes
  forM_ (zip (reverse args) argOffsets)
    (\(argName, i) -> modifyOffsets (M.insert argName i))
  let varSizes = 1 <$ varDecls
  let varOffsets = computeOffsets varSizes 1
  -- Arguments at positive offsets from MP, pointing to top
  varDeclsProgram <- concatMapM codegenLocalVarDecl (zip varOffsets varDecls)
  stmtsProgram <- concatMapM codegenStmt stmts
  pure $ Label funName : Link (sum varSizes) : varDeclsProgram ++ stmtsProgram

codegenStmt :: Stmt UType -> Codegen SSMProgram
codegenStmt (If _ cond thenStmts elseStmts) = do
  conditionProgram <- codegenExpr cond
  thenProgram <- concatMapM codegenStmt thenStmts
  elseProgram <- concatMapM codegenStmt elseStmts
  elseLabel <- freshLabel "else"
  endLabel <- freshLabel "endif"
  pure $
    conditionProgram ++ [BranchFalse elseLabel]
    ++ thenProgram ++ [BranchAlways endLabel, Label elseLabel]
    ++ elseProgram ++ [Label endLabel]

codegenStmt (While _ cond loopStmts) = do
  condProgram <- codegenExpr cond
  loopProgram <- concatMapM codegenStmt loopStmts
  topLabel <- freshLabel "while"
  endLabel <- freshLabel "endwhile"
  pure $
    [Label topLabel] ++ condProgram ++ [BranchFalse endLabel]
    ++ loopProgram ++ [BranchAlways topLabel]
    ++ [Label endLabel]

codegenStmt (Assign _ varLookup varType expr) = do
  exprProgram <- codegenExpr expr
  (addrProgram,_,_) <- go varLookup varType
  pure $ exprProgram ++ addrProgram ++ [StoreAddress 0]
  where
    -- Traverse field selectors "inside-out", i.e. on recursive ascent
    go :: VarLookup -> UType -> Codegen (SSMProgram, UType, T.Text)
    go (VarId _ ident) varTy = do
      location <- lookupLoc ident
      case location of
        Offset offset -> pure ([LoadReg MarkPointer, AddOffset offset], varTy, ident)
        HeapLoc loc -> pure ([LoadConst loc], varTy, ident)
    go (VarField _ varLkp field) varTy = do
      (program,ty,ident) <- go varLkp varTy
      case (field,ty) of
        (Head, TI.List elemTy) -> -- Load segment address, move pointer to head
          pure (program ++ [LoadAddress 0, AddOffset $ -1], elemTy, ident)
        (Tail, TI.List _) -> -- Load segment address, pointer already at tail
          pure (program ++ [LoadAddress 0], ty, ident)
        (Fst, TI.Prod ty1 _) -> -- Load tuple address, move to first component
          pure (program ++ [LoadAddress 0, AddOffset $ -1], ty1, ident)
        (Snd, TI.Prod _ ty2) -> -- Load tuple address, alread at second component
          pure (program ++ [LoadAddress 0], ty2, ident)
        (_,_) -> error $ "Cannot assign to field " <> show field <>
          " of variable " <> show ident <> " with type " <> show ty

codegenStmt (FunCall _ funName args) = do
  argsProgram <- concatMapM codegenExpr args
  let argsSize = length args
  pure $ argsProgram ++
    case funName of -- Relinquish args on stack after returning
      Name name -> [BranchSubr name, Adjust $ negate argsSize]
      _ -> funName2Program funName (getTypeExpr <$> args)

codegenStmt (Return _ Nothing) = pure [Unlink, Ret]
codegenStmt (Return _ (Just expr)) = do
  exprProgram <- codegenExpr expr
  pure $ exprProgram ++ [StoreReg RetReg, Unlink, Ret]


codegenExpr :: Expr UType -> Codegen SSMProgram
codegenExpr (Ident _ ident ty) = loadIdent ident ty
codegenExpr (Int _ i TI.Int) = pure [LoadConst i]
codegenExpr (Char _ c TI.Char) = pure [LoadConst $ fromEnum c]
codegenExpr (Bool _ True TI.Bool) = pure [LoadConst (-1)]
codegenExpr (Bool _ False TI.Bool) = pure [LoadConst 0]

-- Call to subroutine
codegenExpr (FunCallE _ (Name name) args _) = do
  let argsSize = length args
  argsProgram <- concatMapM codegenExpr args
  pure $ argsProgram ++ BranchSubr name :
    [Adjust $ negate argsSize, LoadReg RetReg]

-- Call to primitive operation
codegenExpr (FunCallE _ funName args _) = do
  argsProgram <- concatMapM codegenExpr args
  pure $ argsProgram ++ funName2Program funName (getTypeExpr <$> args)

-- Empty list is represented by address 0xF0F0F0F0
codegenExpr (EmptyList _ (TI.List _)) = pure [LoadConst nullPtr]

-- Compute tuple entries, store to heap
codegenExpr (Tuple _ e1 e2 (TI.Prod _ _)) = do
  e1Program <- codegenExpr e1
  e2Program <- codegenExpr e2
  pure $ e1Program ++ e2Program ++ [StoreHeapMulti 2]

codegenExpr e = error $ "Found expression " <> show e <>
  " with invalid type " <> show (getTypeExpr e)


--------------------------
-- Primitive operations

funName2Program :: FunName -> [UType] -> SSMProgram
funName2Program (Name ident) _ =
  error $ "funName2Program was called with Name " <> T.unpack ident
funName2Program (Constr name) _ = error $ "Code gen missing for constructor `" <> T.unpack name <> "`"
funName2Program (Selector name) _ = error $ "Code gen missing for selector `" <> T.unpack name <> "`"
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
-- List operators
funName2Program Cons [_, TI.List _] = [StoreHeapMulti 2]
funName2Program Cons _ = error "Called `:` with non-list"
funName2Program IsEmpty _ = [LoadConst nullPtr, EqOp]
-- Move SP to end of head
funName2Program HeadFun [TI.List _] = [loadWithOffset $ -1]
funName2Program HeadFun _ = error "Called `hd` on non-list"
funName2Program TailFun [TI.List _] = [loadWithOffset 0]
funName2Program TailFun _ = error "Called `tl` on non-list"
-- Move SP to end of first component
funName2Program FstFun [TI.Prod _ _] = [loadWithOffset $ -1]
funName2Program FstFun _ = error "Called `fst` on non-tuple"
-- Shift second component upwards by size of first component
funName2Program SndFun [TI.Prod _ _] = [loadWithOffset 0]
funName2Program SndFun _ = error "Called `snd` on non-tuple"

funName2Program Print [ty] = case ty of
  TI.Int -> [TrapInt]
  TI.Char -> [TrapChar]
  -- TI.Bool -> [BranchSubr "printBool", Adjust (-1)]
  _ -> error $ "Printing for type " <> show ty <> " not yet implemented!"

funName2Program Print _ = error "Function `print` called with multiple args!"
