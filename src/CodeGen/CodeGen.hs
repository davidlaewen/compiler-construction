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
import Syntax.TypeAST (VarDecl(..), FunMutDecl(..), FunDecl(..), Expr(..), Stmt(..), FunName(..), VarLookup(..))
import qualified Syntax.TypeAST as TA
import TypeInference.Definition (UScheme, UType)
import qualified TypeInference.Definition as TI
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

loadIdent :: T.Text -> UType -> Codegen Program
loadIdent ident ty = let size = uTypeSize ty in
  lookupLoc ident >>= \case
    Offset offset -> pure [LoadLocalMulti offset size]
    -- ldmh extends upwards, offset direction upwards
    HeapLoc loc -> pure [ LoadReg HeapLowReg, AddOffset loc,
                          LoadHeapMulti (negate size + 1) size ]

storeToAddress :: UType -> Program
storeToAddress ty = [StoreAddressMulti 0 $ uTypeSize ty]

loadComposite :: Int -> Instr
loadComposite = LoadHeapMulti 0

computeOffsets :: [Int] -> Int -> [Int]
computeOffsets [] _ = []
computeOffsets (x:xs) acc = acc : computeOffsets xs (x + acc)


--------------------------
-- Code generation

codegen :: TA.Program UType UScheme -> Codegen Program
codegen (TA.Program varDecls funDecls) = do
  let varSizes = map (\(VarDecl _ _ _ ty) -> uTypeSize ty) varDecls
  let varOffsets = computeOffsets varSizes 0
  varDeclsProgram <- concatMapM codegenGlobalVarDecl (zip varOffsets varDecls)
  funDeclsProgram <- concatMapM codegenFunMutDecl funDecls
  pure $ varDeclsProgram ++
    -- HP now at start of global vars, copy to R5
    [LoadReg HeapPointer, StoreReg HeapLowReg] ++
    -- Store all global vars to heap, adjust SP
    StoreHeapMulti (sum varSizes) : Adjust (-1) :
      BranchAlways "main" : funDeclsProgram ++ [Halt]

codegenGlobalVarDecl :: (Int, VarDecl UType) -> Codegen Program
codegenGlobalVarDecl (i, VarDecl _ ident e _) = do
  program <- codegenExpr e
  modifyOffsets (M.insert ident i) -- Local offset for use in subsequent decls
  modifyHeapLocs (M.insert ident i)
  pure program
  -- pure $ program ++ [StoreHeapMulti $ uTypeSize ty]

codegenLocalVarDecl :: (Int, VarDecl UType) -> Codegen Program
codegenLocalVarDecl (i, VarDecl _ ident e ty) = do
  program <- codegenExpr e
  modifyOffsets (M.insert ident i)
  -- Store immediately, since subsequent local vars may refer to this decl
  pure $ program ++ [StoreLocalMulti i $ uTypeSize ty]

codegenFunMutDecl :: FunMutDecl UType UScheme -> Codegen Program
codegenFunMutDecl (SingleDecl funDecl) = codegenFunDecl funDecl
codegenFunMutDecl (MutualDecls funDecls) = concatMapM codegenFunDecl funDecls

codegenFunDecl :: FunDecl UType UScheme -> Codegen Program
codegenFunDecl (FunDecl funName args _ varDecls stmts uScheme) = do
  -- TODO: Polymorphic function types
  let argTypes = case uScheme of
        TI.UScheme _ (TI.Fun argTys _) -> argTys
        _ -> error ""
  modifyOffsets (const M.empty)
  let argSizes = reverse $ map uTypeSize argTypes
  -- Offsets should point to top. Negative offsets from MP, reverse order
  let argOffsets = succ <$> zipWith (-) (negate <$> computeOffsets argSizes 2) argSizes
  forM_ (zip (reverse args) argOffsets)
    (\(argName, i) -> modifyOffsets (M.insert argName i))
  let varSizes = map (\(VarDecl _ _ _ ty) -> uTypeSize ty) varDecls
  let varOffsets = computeOffsets varSizes 1
  -- Arguments at positive offsets from MP, pointing to top
  varDeclsProgram <- concatMapM codegenLocalVarDecl (zip varOffsets varDecls)
  stmtsProgram <- concatMapM codegenStmt stmts
  pure $ Label funName : Link (sum varSizes) : varDeclsProgram ++ stmtsProgram

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

codegenStmt (Assign varLookup varType expr) = do
  exprProgram <- codegenExpr expr
  (addrProgram,ty,_) <- go varLookup varType
  pure $ exprProgram ++ addrProgram ++ storeToAddress ty
  where
    -- Traverse field selectors "inside-out", i.e. on recursive ascent
    go :: VarLookup -> UType -> Codegen (Program, UType, T.Text)
    go (VarId ident) varTy = do
      location <- lookupLoc ident
      case location of
        Offset offset -> pure ([LoadReg MarkPointer, AddOffset offset], varTy, ident)
        HeapLoc loc -> pure ([LoadConst loc], varTy, ident)
    go (VarField varLkp field) varTy = do
      (program,ty,ident) <- go varLkp varTy
      case (field,ty) of
        (TA.Head, TI.List elemTy) -> -- Load segment address, move pointer to start of head
          pure (program ++ [LoadAddress 0, AddOffset $ negate (uTypeSize elemTy)], elemTy, ident)
        (TA.Tail, TI.List _) -> -- Load segment address, pointer already at tail
          pure (program ++ [LoadAddress 0], ty, ident)
        (TA.Fst, TI.Prod ty1 _) -> -- Load tuple address, move to start of fst component
          pure (program ++ [LoadAddress 0, AddOffset $ negate (uTypeSize ty1)], ty1, ident)
        (TA.Snd, TI.Prod _ ty2) -> -- Load tuple address, yields start of snd component
          pure (program ++ [LoadAddress 0], ty2, ident)
        (_,_) -> error $ "Cannot assign to field " <> show field <>
          " of variable " <> show ident <> " with type " <> show ty

codegenStmt (FunCall funName args) = do
  argsProgram <- concatMapM codegenExpr args
  let argsSize = sum $ uTypeSize . TA.getTypeExpr <$> args
  pure $ argsProgram ++
    case funName of -- Relinquish args on stack after returning
      Name name -> [BranchSubr name, Adjust $ negate argsSize]
      _ -> funName2Program funName (TA.getTypeExpr <$> args)

codegenStmt (Return Nothing) = pure [Unlink, Ret]
codegenStmt (Return (Just expr)) = do
  let retSize = uTypeSize $ TA.getTypeExpr expr
  exprProgram <- codegenExpr expr
  let storeProgram = case retSize of
        1 -> []
        _ -> [StoreHeapMulti retSize]
  pure $ exprProgram ++ storeProgram ++ [StoreReg RetReg, Unlink, Ret]


codegenExpr :: Expr UType -> Codegen Program
codegenExpr (TA.Ident ident ty) = loadIdent ident ty
codegenExpr (TA.Int i TI.Int) = pure [LoadConst i]
codegenExpr (TA.Char c TI.Char) = pure [LoadConst $ fromEnum c]
codegenExpr (TA.Bool True TI.Bool) = pure [LoadConst (-1)]
codegenExpr (TA.Bool False TI.Bool) = pure [LoadConst 0]

-- Call to subroutine
codegenExpr (FunCallE (Name name) args retType) = do
  let argsSize = sum $ uTypeSize . TA.getTypeExpr <$> args
  let retSize = uTypeSize retType
  argsProgram <- concatMapM codegenExpr args
  let unboxProgram = [LoadHeapMulti 0 retSize | retSize > 1]
  pure $ argsProgram ++ BranchSubr name :
    [Adjust $ negate argsSize, LoadReg RetReg] ++ unboxProgram

-- Call to primitive operation
codegenExpr (FunCallE funName args _) = do
  argsProgram <- concatMapM codegenExpr args
  pure $ argsProgram ++ funName2Program funName (TA.getTypeExpr <$> args)

-- Empty list is represented by address 0xF0F0F0F0
codegenExpr (EmptyList (TI.List _)) = pure [LoadConst nullPtr]

-- Compute tuple entries, store to heap
codegenExpr (TA.Tuple e1 e2 (TI.Prod ty1 ty2)) = do
  e1Program <- codegenExpr e1
  e2Program <- codegenExpr e2
  pure $ e1Program ++ e2Program ++
    [StoreHeapMulti $ uTypeSize ty1 + uTypeSize ty2]

codegenExpr e = error $ "Found expression " <> show e <>
  " with invalid type " <> show (TA.getTypeExpr e)


--------------------------
-- Primitive operations

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
-- List operators
funName2Program Cons [ty, TI.List _] = [StoreHeapMulti $ uTypeSize ty + 1]
funName2Program Cons _ = error "Called `:` with non-list"
funName2Program IsEmpty _ = [LoadConst nullPtr, EqOp]
-- Move SP to end of head
funName2Program HeadFun [TI.List ty] = loadComposite size : [Adjust $ -1]
  where size = uTypeSize ty + 1
funName2Program HeadFun _ = error "Called `hd` on non-list"
-- Shift tail upwards by size of head
funName2Program TailFun [TI.List ty] =
  loadComposite (size + 1) : [ StoreStack $ negate size, Adjust $ 1 - size ]
  where size = uTypeSize ty
funName2Program TailFun _ = error "Called `tl` on non-list"
-- Move SP to end of first component
funName2Program FstFun [TI.Prod ty1 ty2] =
  loadComposite size : [Adjust $ negate $ uTypeSize ty2]
  where size = uTypeSize ty1 + uTypeSize ty2
funName2Program FstFun _ = error "Called `fst` on non-tuple"
-- Shift second component upwards by size of first component
funName2Program SndFun [TI.Prod ty1 ty2] =
  let size1 = uTypeSize ty1
      size2 = uTypeSize ty2
      offset = negate $ size1 + size2 - 1 in
  loadComposite (size1 + size2) :
    [ StoreStackMulti offset size2, Adjust $ size2 - size1 ]
funName2Program SndFun _ = error "Called `snd` on non-tuple"

funName2Program Print [ty] = case ty of
  TI.Int -> [TrapInt]
  TI.Char -> [TrapChar]
  -- TI.Bool -> [BranchSubr "printBool", Adjust (-1)]
  _ -> error $ "Printing for type " <> show ty <> " not yet implemented!"

funName2Program Print _ = error "Function `print` called with multiple args!"
