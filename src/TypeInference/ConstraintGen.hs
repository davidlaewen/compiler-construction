{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TypeInference.ConstraintGen (checkProgram) where

import TypeInference.Definition
import TypeInference.Unify
import TypeInference.Annotate (annotateFunDecl)
import qualified Syntax.TypeAST as T
import Control.Monad.State
import Data.Text (pack)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as Text
import Utils.Loc (Loc)

checkProgram :: T.Program () () -> CGen (T.Program UType UScheme, Subst)
checkProgram (T.Program dataDecls varDecls funDecls) = do
  mapM_ checkDataDecl dataDecls
  (vds,s1) <- checkVarDecls GlobalLevel varDecls
  (fds,s2) <- checkList checkFunMutDecl funDecls
  pure (T.Program dataDecls vds fds, s1 <> s2)

checkList :: (a -> CGen (b, Subst)) -> [a] -> CGen ([b], Subst)
checkList _ [] = pure ([], mempty)
checkList f (x:xs) = do
  (x',s) <- f x
  (xs',ss) <- checkList f xs
  pure (x':xs', ss <> s)

checkDataDecl :: T.DataDecl -> CGen ()
checkDataDecl (T.DataDecl _ name constrs) = do
  mapM_ (checkDataConstr name) constrs

checkDataConstr :: Text.Text -> T.DataConstr -> CGen ()
checkDataConstr declName (T.DataConstr _ cName args) = do
  -- Quantify over type variables to support polymorphism
  envInsertConstr cName $ UScheme S.empty (Fun (snd <$> args) (Data declName))
  forM_ args (\(selName,ty) -> -- Insert selector schemes
    envInsertSelector selName $ UScheme S.empty (Fun [Data declName] ty))

checkVarDecls :: EnvLevel -> [T.VarDecl ()] -> CGen ([T.VarDecl UType], Subst)
checkVarDecls envLevel = checkList $ checkVarDecl envLevel

checkVarDecl :: EnvLevel -> T.VarDecl () -> CGen (T.VarDecl UType, Subst)
checkVarDecl envLevel (T.VarDecl loc mTy name expr _) = do
  uTy <- case mTy of
    Nothing -> UVar <$> freshVar
    Just ty -> pure ty
  (expr', exprType, exprSubst) <- checkExpr expr
  envInsertVar envLevel name uTy
  s <- unify exprType uTy loc
  applySubst s
  pure (T.VarDecl loc mTy name expr' uTy, s <> exprSubst)

checkFunMutDecl :: T.FunMutDecl () () -> CGen (T.FunMutDecl UType UScheme, Subst)
checkFunMutDecl (T.SingleDecl funDecl) =
  checkFunDecl funDecl >>= \(fd, s) -> do
    fd' <- generaliseFunScheme fd
    pure (T.SingleDecl fd',s)

checkFunMutDecl (T.MutualDecls loc funDecls) = do
  -- Generate schemes with placeholder uvars for all functions in block
  forM_ funDecls $ \(T.FunDecl _ name params _ _ _ _) -> do
    (paramUVars,retUVar) <- genFunDeclTypes name params
    envGlobalInsertFun name $ UScheme S.empty $ Fun paramUVars retUVar
  -- Check fun decls in order without generalising
  (fds,ss) <- checkList checkFunDecl funDecls
  -- Substitute and generalise all function schemes
  let fds' = map (annotateFunDecl ss) fds
  -- let fds' = map (subst ss) fds
  fds'' <- forM fds' generaliseFunScheme
  pure (T.MutualDecls loc fds'', ss)

generaliseFunScheme :: T.FunDecl UType UScheme -> CGen (T.FunDecl UType UScheme)
generaliseFunScheme (T.FunDecl loc name params mTy varDecls stmts (UScheme _ funTy)) = do
  -- Generalise over free uvars
  let binders = freeUVars funTy
  let funScheme = UScheme binders funTy
  -- Update function scheme in environment
  envGlobalInsertFun name funScheme
  pure $ T.FunDecl loc name params mTy varDecls stmts funScheme

genFunDeclTypes :: Text.Text -> [a] -> CGen ([UType],UType)
genFunDeclTypes name params = do
  mFunScheme <- envLookupFun name
  case mFunScheme of -- Check for existing entry
    Just (UScheme _ (Fun argTys retTy)) -> pure (argTys,retTy)
    Just _ -> error $ -- Invalid env entry, internal error
      "Found invalid scheme for function " <> Text.unpack name <> " in global env"
    Nothing -> do -- Generate fresh uvars
      uVarsParams <- forM params (const $ UVar <$> freshVar)
      retTy <- UVar <$> freshVar
      pure (uVarsParams,retTy)

checkFunDecl :: T.FunDecl () () -> CGen (T.FunDecl UType UScheme, Subst)
checkFunDecl (T.FunDecl loc name params mTy varDecls stmts _) = do
  -- Generate uvars for params and return type, add to local environment
  (uVarsParams,uVarRet) <- genFunDeclTypes name params
  let funTy = Fun uVarsParams uVarRet
  forM_ (zip params uVarsParams) $
    uncurry (envInsertVar LocalLevel)
  envInsertRetType uVarRet
  envLocalInsertFun name funTy
  -- Check var decls and statements
  (varDecls',varDeclsSubst) <- checkVarDecls LocalLevel varDecls
  (stmts',stmtsSubst) <- checkStmts stmts
  clearLocalEnv
  -- Check user-specified type against inferred type
  tySubst <- case mTy of
    Nothing -> pure mempty
    Just userTy -> do -- Substitute uvars for tvars in annotated type
      userTy' <- instantiateUserType userTy
      unify funTy userTy' loc
  applySubst tySubst
  let s = stmtsSubst <> varDeclsSubst <> tySubst
  let ty = subst s $ Fun uVarsParams uVarRet
  let funScheme = UScheme S.empty ty
  envGlobalInsertFun name funScheme
  pure (T.FunDecl loc name params mTy varDecls' stmts' funScheme, s)

instantiateUserType :: UType -> CGen UType
instantiateUserType ty = do
  let tVars = freeTVars ty
  s <- sequence $ M.fromSet (const freshVar) tVars
  pure $ substTVars s ty


checkStmts :: [T.Stmt ()] -> CGen ([T.Stmt UType], Subst)
checkStmts = checkList checkStmt

checkStmt :: T.Stmt () -> CGen (T.Stmt UType, Subst)
checkStmt (T.If loc condExpr thenStmts elseStmts) = do
  (condExpr', condExprType, condExprSubst) <- checkExpr condExpr
  s <- unify condExprType Bool loc
  applySubst s
  (thenStmts', thenStmtsSubst) <- checkStmts thenStmts
  (elseStmts', elseStmtsSubst) <- checkStmts elseStmts
  pure (T.If loc condExpr' thenStmts' elseStmts',
    elseStmtsSubst <> thenStmtsSubst <> s <> condExprSubst)

checkStmt (T.While loc condExpr loopStmts) = do
  (condExpr', condExprType, condExprSubst) <- checkExpr condExpr
  s <- unify condExprType Bool loc
  applySubst s
  (loopStmts', loopStmtsSubst) <- checkStmts loopStmts
  pure (T.While loc condExpr' loopStmts', loopStmtsSubst <> s <> condExprSubst)

checkStmt (T.Assign loc varLookup _ expr) = do
  (expr', exprType, exprSubst) <- checkExpr expr
  (varTy,s) <- foo varLookup exprType
  pure (T.Assign loc varLookup varTy expr', s <> exprSubst)
  where
    foo :: T.VarLookup -> UType -> CGen (UType, Subst)
    foo (T.VarId loc' name) exprType = do
      mVarType <- envLookupVar name
      case mVarType of
        Nothing -> throwLocError loc' $ "No variable declaration matching `" <> name <> "`"
        Just varType -> do
          s <- unify exprType varType loc
          applySubst s
          pure (varType,s)
    foo (T.VarField _ varLkp field) exprType = do
      case field of
        T.Head -> foo varLkp (List exprType)
        T.Tail -> foo varLkp exprType
        T.Fst -> do
          uVarSnd <- UVar <$> freshVar
          foo varLkp (Prod exprType uVarSnd)
        T.Snd -> do
          uVarFst <- UVar <$> freshVar
          foo varLkp (Prod uVarFst exprType)

checkStmt (T.FunCall loc funName args) = do
  (args',_,s) <- checkFunCall funName args loc
  pure (T.FunCall loc funName args', s)

checkStmt (T.Return loc mExpr) = do
  mRetType <- envLookupRetType
  case mRetType of
    Nothing -> error "RetType not found in environment!"
    Just retType -> do
      case mExpr of
        Nothing -> do
          s <- unify retType Void loc
          applySubst s
          pure (T.Return loc Nothing, s)
        Just expr -> do
          (expr', exprType, exprSubst) <- checkExpr expr
          s <- unify retType exprType loc
          applySubst s
          pure (T.Return loc (Just expr'), s <> exprSubst)


checkExpr :: T.Expr () -> CGen (T.Expr UType, UType, Subst)
checkExpr (T.Ident loc name _) = do
  -- TODO: We need a way to check whether this a local or global variable
  envLookupVar name >>= \case
      Nothing -> throwLocError loc $ "Could not find variable `" <> name <> "`"
      Just ut -> pure (T.Ident loc name ut, ut, mempty)
checkExpr (T.Int loc n _) = pure (T.Int loc n Int, Int, mempty)
checkExpr (T.Char loc c _) = pure (T.Char loc c Char, Char, mempty)
checkExpr (T.Bool loc b _) = pure (T.Bool loc b Bool, Bool, mempty)
checkExpr (T.FunCallE loc funName args _) = do
  (args', retType, s) <- checkFunCall funName args loc
  pure (T.FunCallE loc funName args' retType, retType, s)
checkExpr (T.EmptyList loc _) = do
  ty <- List . UVar <$> freshVar
  pure (T.EmptyList loc ty, ty, mempty)
checkExpr (T.Tuple loc e1 e2 _) = do
  (e1',t1,s1) <- checkExpr e1
  (e2',t2,s2) <- checkExpr e2
  let ty = Prod t1 t2
  pure (T.Tuple loc e1' e2' ty, ty, s2 <> s1)

checkExprs :: [T.Expr ()] -> CGen ([T.Expr UType], [UType], Subst)
checkExprs [] = pure ([],[],mempty)
checkExprs (expr:exprs) = do
  (expr', exprType, exprSubst) <- checkExpr expr
  (exprs', exprsTypes, exprsSubst) <- checkExprs exprs
  pure (expr':exprs', exprType:exprsTypes, exprSubst <> exprsSubst)

checkFunCall :: T.FunName -> [T.Expr ()] -> Loc -> CGen ([T.Expr UType], UType, Subst)
checkFunCall funName args loc = do
  funType <- getFunType funName loc >>= instantiateScheme
  case funType of
    Fun paramTypes retType -> do
      (args', argsTypes, argsSubst) <- checkExprs args
      s <- unifyLists paramTypes argsTypes
      applySubst s
      pure (args', subst s retType, s <> argsSubst)
    _ -> error $ -- Invalid global env entry, internal error
      "Function " <> show funName <> " does not have function type"
  where
    unifyLists :: [UType] -> [UType] -> CGen Subst
    unifyLists [] [] = pure mempty
    unifyLists (ty1:tys1) (ty2:tys2) = do
      s <- unify ty1 ty2 loc
      ss <- unifyLists (subst s <$> tys1) (subst s <$> tys2)
      pure $ ss <> s
    unifyLists _ _ =
      throwLocError loc $ "Incorrect number of arguments in call to " <> pack (show funName)

instantiateScheme :: UScheme -> CGen UType
instantiateScheme (UScheme tVars ty) = do
  s <- sequence $ M.fromSet (const $ UVar <$> freshVar) tVars
  pure $ subst (Subst s) ty

getFunType :: T.FunName -> Loc -> CGen UScheme
getFunType (T.Name name) loc = do
    mFunType <- envLookupFun name
    case mFunType of
      Nothing -> throwLocError loc $ "No declaration for function `" <> name <> "`"
      Just funType -> pure funType
getFunType (T.Constr name) loc = envLookupConstr name >>= \case
    Just constrTy -> pure constrTy
    Nothing -> throwLocError loc $ "No declaration for constructor `" <> name <> "`"
getFunType (T.Selector name) loc = envLookupSelector name >>= \case
    Just selTy -> pure selTy
    Nothing -> throwLocError loc $ "No declaration for selector `" <> name <> "`"
getFunType T.Not     _ = pure $ UScheme S.empty (Fun [Bool] Bool)
getFunType T.Neg     _ = pure $ UScheme S.empty (Fun [Int] Int)
getFunType T.Add     _ = pure $ UScheme S.empty (Fun [Int, Int] Int)
getFunType T.Sub     _ = pure $ UScheme S.empty (Fun [Int, Int] Int)
getFunType T.Mul     _ = pure $ UScheme S.empty (Fun [Int, Int] Int)
getFunType T.Div     _ = pure $ UScheme S.empty (Fun [Int, Int] Int)
getFunType T.Mod     _ = pure $ UScheme S.empty (Fun [Int, Int] Int)
getFunType T.Eq      _ = pure $ UScheme (S.singleton 0) (Fun [UVar 0, UVar 0] Bool) -- TODO: We probably want an Eq type class for this
getFunType T.Neq     _ = pure $ UScheme (S.singleton 0) (Fun [UVar 0, UVar 0] Bool) -- Same here
getFunType T.Lt      _ = pure $ UScheme S.empty (Fun [Int,Int] Bool)
getFunType T.Gt      _ = pure $ UScheme S.empty (Fun [Int,Int] Bool)
getFunType T.Lte     _ = pure $ UScheme S.empty (Fun [Int,Int] Bool)
getFunType T.Gte     _ = pure $ UScheme S.empty (Fun [Int,Int] Bool)
getFunType T.And     _ = pure $ UScheme S.empty (Fun [Bool,Bool] Bool)
getFunType T.Or      _ = pure $ UScheme S.empty (Fun [Bool,Bool] Bool)
getFunType T.Cons    _ = pure $ UScheme (S.singleton 0) (Fun [UVar 0, List (UVar 0)] (List (UVar 0)))
getFunType T.IsEmpty _ = pure $ UScheme (S.singleton 0) (Fun [List (UVar 0)] Bool)
getFunType T.Print   _ = pure $ UScheme (S.singleton 0) (Fun [UVar 0] Void)
getFunType T.HeadFun _ = pure $ UScheme (S.singleton 0) (Fun [List $ UVar 0] $ UVar 0)
getFunType T.TailFun _ = pure $ UScheme (S.singleton 0) (Fun [List $ UVar 0] (List (UVar 0)))
getFunType T.FstFun  _ = pure $ UScheme (S.fromList [0, 1]) (Fun [Prod (UVar 0) (UVar 1)] (UVar 0))
getFunType T.SndFun  _ = pure $ UScheme (S.fromList [0, 1]) (Fun [Prod (UVar 0) (UVar 1)] (UVar 1))
