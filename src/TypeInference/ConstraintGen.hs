{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TypeInference.ConstraintGen (checkProgram) where

import TypeInference.Definition
import TypeInference.Unify
import qualified Syntax.TypeAST as T
import Control.Monad.State
import Control.Monad.Except ( MonadError(throwError) )
import Data.Text (pack)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as Text

checkProgram :: T.Program () () -> CGen (T.Program UType UScheme, Subst)
checkProgram (T.Program varDecls funDecls) = do
  (vds,s1) <- checkVarDecls GlobalLevel varDecls
  (fds,s2) <- checkList checkFunMutDecl funDecls
  pure (T.Program vds fds, s1 <> s2)

checkList :: (a -> CGen (b, Subst)) -> [a] -> CGen ([b], Subst)
checkList _ [] = pure ([], mempty)
checkList f (x:xs) = do
  (x',s) <- f x
  (xs',ss) <- checkList f xs
  pure (x':xs', ss <> s)

checkVarDecls :: EnvLevel -> [T.VarDecl ()] -> CGen ([T.VarDecl UType], Subst)
checkVarDecls envLevel = checkList $ checkVarDecl envLevel

checkVarDecl :: EnvLevel -> T.VarDecl () -> CGen (T.VarDecl UType, Subst)
checkVarDecl envLevel (T.VarDecl mTy name expr _) = do
  uTy <- case mTy of
    Nothing -> UVar <$> freshVar
    Just ty -> pure ty
  (expr', exprType, exprSubst) <- checkExpr expr
  envInsertVar envLevel name uTy
  s <- unify exprType uTy
  applySubst s
  pure (T.VarDecl mTy name expr' uTy, s <> exprSubst)

checkFunMutDecl :: T.FunMutDecl () () -> CGen (T.FunMutDecl UType UScheme, Subst)
checkFunMutDecl (T.SingleDecl funDecl) =
  checkFunDecl funDecl >>= \(fd, s) -> do
    fd' <- generaliseFunScheme fd
    pure (T.SingleDecl fd',s)

checkFunMutDecl (T.MutualDecls funDecls) = do
  -- Generate schemes with placeholder uvars for all functions in block
  forM_ funDecls $ \(T.FunDecl name params _ _ _ _) -> do
    (paramUVars,retUVar) <- genFunDeclTypes name params
    envGlobalInsertFun name $ UScheme S.empty $ Fun paramUVars retUVar
  -- Check fun decls in order without generalising
  (fds,ss) <- checkList checkFunDecl funDecls
  -- Substitute and generalise all function schemes
  let fds' = map (subst ss) fds
  fds'' <- forM fds' generaliseFunScheme
  pure (T.MutualDecls fds'', ss)

generaliseFunScheme :: T.FunDecl UType UScheme -> CGen (T.FunDecl UType UScheme)
generaliseFunScheme (T.FunDecl name params mTy varDecls stmts (UScheme _ funTy)) = do
  -- Generalise over free uvars
  let binders = freeUVars funTy
  let funScheme = UScheme binders funTy
  -- Update function scheme in environment
  envGlobalInsertFun name funScheme
  pure $ T.FunDecl name params mTy varDecls stmts funScheme

genFunDeclTypes :: Text.Text -> [a] -> CGen ([UType],UType)
genFunDeclTypes name params = do
  mFunScheme <- envLookupFun name
  case mFunScheme of -- Check for existing entry
    Just (UScheme _ (Fun argTys retTy)) -> pure (argTys,retTy)
    Just _ -> error $
      "Found invalid scheme for function " <> Text.unpack name <> " in global env"
    Nothing -> do -- Generate fresh uvars
      uVarsParams <- forM params (const $ UVar <$> freshVar)
      retTy <- UVar <$> freshVar
      pure (uVarsParams,retTy)

checkFunDecl :: T.FunDecl () () -> CGen (T.FunDecl UType UScheme, Subst)
checkFunDecl (T.FunDecl name params mTy varDecls stmts _) = do
  -- Generate uvars for params and return type, add to environment
  (uVarsParams,retTy) <- genFunDeclTypes name params
  forM_ (zip params uVarsParams) $
    uncurry (envInsertVar LocalLevel)
  envInsertRetType retTy
  envLocalInsertFun name (Fun uVarsParams retTy)
  -- Check var decls and statements
  (varDecls',varDeclsSubst) <- checkVarDecls LocalLevel varDecls
  (stmts',stmtsSubst) <- checkStmts stmts
  clearLocalEnv
  -- TODO: Check user-specified type against inferred type
  let s = stmtsSubst <> varDeclsSubst
  let funTy = subst s $ Fun uVarsParams retTy
  let funScheme = UScheme S.empty funTy
  envGlobalInsertFun name funScheme
  pure (T.FunDecl name params mTy varDecls' stmts' funScheme, s)

checkStmts :: [T.Stmt ()] -> CGen ([T.Stmt UType], Subst)
checkStmts = checkList checkStmt

checkStmt :: T.Stmt () -> CGen (T.Stmt UType, Subst)
checkStmt (T.If condExpr thenStmts elseStmts) = do
  (condExpr', condExprType, condExprSubst) <- checkExpr condExpr
  s <- unify condExprType Bool
  applySubst s
  (thenStmts', thenStmtsSubst) <- checkStmts thenStmts
  (elseStmts', elseStmtsSubst) <- checkStmts elseStmts
  pure (T.If condExpr' thenStmts' elseStmts',
    elseStmtsSubst <> thenStmtsSubst <> s <> condExprSubst)

checkStmt (T.While condExpr loopStmts) = do
  (condExpr', condExprType, condExprSubst) <- checkExpr condExpr
  s <- unify condExprType Bool
  applySubst s
  (loopStmts', loopStmtsSubst) <- checkStmts loopStmts
  pure (T.While condExpr' loopStmts', loopStmtsSubst <> s <> condExprSubst)

checkStmt (T.Assign varLookup _ expr) = do
  (expr', exprType, exprSubst) <- checkExpr expr
  (varTy,s) <- foo varLookup exprType
  pure (T.Assign varLookup varTy expr', s <> exprSubst)
  where
    foo :: T.VarLookup -> UType -> CGen (UType, Subst)
    foo (T.VarId name) exprType = do
      mVarType <- envLookupVar name
      case mVarType of
        Nothing -> throwError $ "No variable declaration matching " <> name
        Just varType -> do
          s <- unify exprType varType
          applySubst s
          pure (varType,s)
    foo (T.VarField varLkp field) exprType = do
      case field of
        T.Head -> foo varLkp (List exprType)
        T.Tail -> foo varLkp exprType
        T.Fst -> do
          uVarSnd <- UVar <$> freshVar
          foo varLkp (Prod exprType uVarSnd)
        T.Snd -> do
          uVarFst <- UVar <$> freshVar
          foo varLkp (Prod uVarFst exprType)

checkStmt (T.FunCall funName args) = do
  (args',_,s) <- checkFunCall funName args
  pure (T.FunCall funName args', s)

checkStmt (T.Return mExpr) = do
  mRetType <- envLookupRetType
  case mRetType of
    Nothing -> error "RetType not found in environment!"
    Just retType -> do
      case mExpr of
        Nothing -> do
          s <- unify retType Void
          applySubst s
          pure (T.Return Nothing, s)
        Just expr -> do
          (expr', exprType, exprSubst) <- checkExpr expr
          s <- unify retType exprType
          applySubst s
          pure (T.Return (Just expr'), s <> exprSubst)


checkExpr :: T.Expr () -> CGen (T.Expr UType, UType, Subst)
checkExpr (T.Ident name _) = do
  -- TODO: We need a way to check whether this a local or global variable
  envLookupVar name >>=
    \case
      Nothing -> throwError $ "Could not find variable `" <> name <> "`"
      Just ut -> pure (T.Ident name ut, ut, mempty)
checkExpr (T.Int n _) = pure (T.Int n Int, Int, mempty)
checkExpr (T.Char c _) = pure (T.Char c Char, Char, mempty)
checkExpr (T.Bool b _) = pure (T.Bool b Bool, Bool, mempty)
checkExpr (T.FunCallE funName args _) = do
  (args', retType, s) <- checkFunCall funName args
  pure (T.FunCallE funName args' retType, retType, s)
checkExpr (T.EmptyList _) = do
  ty <- List . UVar <$> freshVar
  pure (T.EmptyList ty, ty, mempty)
checkExpr (T.Tuple e1 e2 _) = do
  (e1',t1,s1) <- checkExpr e1
  (e2',t2,s2) <- checkExpr e2
  let ty = Prod t1 t2
  pure (T.Tuple e1' e2' ty, ty, s2 <> s1)

checkExprs :: [T.Expr ()] -> CGen ([T.Expr UType], [UType], Subst)
checkExprs [] = pure ([],[],mempty)
checkExprs (expr:exprs) = do
  (expr', exprType, exprSubst) <- checkExpr expr
  (exprs', exprsTypes, exprsSubst) <- checkExprs exprs
  pure (expr':exprs', exprType:exprsTypes, exprSubst <> exprsSubst)

checkFunCall :: T.FunName -> [T.Expr ()] -> CGen ([T.Expr UType], UType, Subst)
checkFunCall funName args = do
  funType <- getFunType funName >>= instantiateScheme
  case funType of
    Fun paramTypes retType -> do
      (args', argsTypes, argsSubst) <- checkExprs args
      s <- unifyLists paramTypes argsTypes
      applySubst s
      pure (args', subst s retType, s <> argsSubst)
    _ -> throwError $ "Function " <> pack (show funName) <> " does not have a function type"
  where
    unifyLists :: [UType] -> [UType] -> CGen Subst
    unifyLists [] [] = pure mempty
    unifyLists (ty1:tys1) (ty2:tys2) = do
      s <- unify ty1 ty2
      ss <- unifyLists (subst s <$> tys1) (subst s <$> tys2)
      pure $ ss <> s
    unifyLists _ _ =
      throwError $ "Incorrect number of arguments in call to " <> pack (show funName)

instantiateScheme :: UScheme -> CGen UType
instantiateScheme (UScheme tVars ty) = do
  s <- sequence $ M.fromSet (const $ UVar <$> freshVar) tVars
  pure $ subst (Subst s) ty

getFunType :: T.FunName -> CGen UScheme
getFunType funName = do
  case funName of
    T.Name name -> do
      mFunType <- envLookupFun name
      case mFunType of
        Nothing -> throwError $ "No declaration for function " <> name
        Just funType -> pure funType
    T.Not -> pure $ UScheme S.empty (Fun [Bool] Bool)
    T.Neg -> pure $ UScheme S.empty (Fun [Int] Int)
    T.Add -> pure $ UScheme S.empty (Fun [Int, Int] Int)
    T.Sub -> pure $ UScheme S.empty (Fun [Int, Int] Int)
    T.Mul -> pure $ UScheme S.empty (Fun [Int, Int] Int)
    T.Div -> pure $ UScheme S.empty (Fun [Int, Int] Int)
    T.Mod -> pure $ UScheme S.empty (Fun [Int, Int] Int)
    T.Eq -> pure $ UScheme (S.singleton 0) (Fun [UVar 0, UVar 0] Bool) -- TODO: We probably want an Eq type class for this
    T.Neq -> pure $ UScheme (S.singleton 0) (Fun [UVar 0, UVar 0] Bool) -- Same here
    T.Lt -> pure $ UScheme S.empty (Fun [Int,Int] Bool)
    T.Gt -> pure $ UScheme S.empty (Fun [Int,Int] Bool)
    T.Lte -> pure $ UScheme S.empty (Fun [Int,Int] Bool)
    T.Gte -> pure $ UScheme S.empty (Fun [Int,Int] Bool)
    T.And -> pure $ UScheme S.empty (Fun [Bool,Bool] Bool)
    T.Or -> pure $ UScheme S.empty (Fun [Bool,Bool] Bool)
    T.Cons -> pure $ UScheme (S.singleton 0) (Fun [UVar 0, List (UVar 0)] (List (UVar 0)))
    T.IsEmpty -> pure $ UScheme (S.singleton 0) (Fun [List (UVar 0)] Bool)
    T.Print -> pure $ UScheme (S.singleton 0) (Fun [UVar 0] Void)
    T.HeadFun -> pure $ UScheme (S.singleton 0) (Fun [List $ UVar 0] $ UVar 0)
    T.TailFun -> pure $ UScheme (S.singleton 0) (Fun [List $ UVar 0] (List (UVar 0)))
    T.FstFun -> pure $ UScheme (S.fromList [0, 1]) (Fun [Prod (UVar 0) (UVar 1)] (UVar 0))
    T.SndFun -> pure $ UScheme (S.fromList [0, 1]) (Fun [Prod (UVar 0) (UVar 1)] (UVar 1))
