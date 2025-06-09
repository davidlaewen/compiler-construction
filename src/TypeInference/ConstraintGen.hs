{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TypeInference.ConstraintGen (checkProgram) where

import TypeInference.Definition
import TypeInference.Unify
import TypeInference.Annotate (annotateFunDecl)
import Syntax.TypeAST hiding (Bool,Char,Int)
import qualified Syntax.TypeAST as TA
import Control.Monad.State
import Data.Text (pack)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as Text
import Utils.Loc (Loc)
import qualified Data.Text as T

checkProgram :: Program () () -> CGen (Program UType UScheme, Subst)
checkProgram (Program dataDecls varDecls funDecls) = do
  mapM_ checkDataDecl dataDecls
  (vds,s1) <- checkVarDecls GlobalLevel varDecls
  (fds,s2) <- checkList checkFunMutDecl funDecls
  pure (Program dataDecls vds fds, s1 <> s2)

checkList :: (a -> CGen (b, Subst)) -> [a] -> CGen ([b], Subst)
checkList _ [] = pure ([], mempty)
checkList f (x:xs) = do
  (x',s) <- f x
  (xs',ss) <- checkList f xs
  pure (x':xs', ss <> s)

checkDataDecl :: DataDecl -> CGen ()
checkDataDecl (DataDecl _ name tyParams ctors) = do
  let uvars    = take (length tyParams) [(0::Int)..]
      uvarSet  = S.fromList uvars
      tvarMap  = M.fromList (zip tyParams uvars)
      dataType = Data name (UVar <$> uvars)
  mapM_ (checkDataCtor dataType uvarSet tvarMap) ctors

checkDataCtor :: UType -> S.Set UVar -> M.Map TVar UVar -> Ctor -> CGen ()
checkDataCtor dataType uvarSet tvarMap (Ctor _ cName args) = do
  -- All type schemes include the data type listing all type arguments, hence we
  -- always quantify over `uvarSet`
  let argTys = substTVars tvarMap . snd <$> args
  envInsertCtor cName $ UScheme uvarSet (Fun argTys dataType)
  -- Insert constructor predicate function of the form `is<CName>` with scheme
  -- `∀ . <DataType> -> Bool`
  envGlobalInsertFun ("is" <> cName) $ UScheme uvarSet (Fun [dataType] Bool)
  forM_ args (\(selName,ty) -> -- Insert selector schemes
    let outTy = substTVars tvarMap ty in
    envInsertSelector selName $ UScheme uvarSet (Fun [dataType] outTy))

checkVarDecls :: EnvLevel -> [VarDecl ()] -> CGen ([VarDecl UType], Subst)
checkVarDecls envLevel = checkList $ checkVarDecl envLevel

checkVarDecl :: EnvLevel -> VarDecl () -> CGen (VarDecl UType, Subst)
checkVarDecl envLevel (VarDecl loc mTy name expr _) = do
  uTy <- case mTy of
    Nothing -> UVar <$> freshVar
    Just ty -> pure ty
  (expr', exprType, exprSubst) <- checkExpr expr
  envInsertVar envLevel name uTy
  s <- unify exprType uTy loc
  applySubst s
  pure (VarDecl loc mTy name expr' uTy, s <> exprSubst)

checkFunMutDecl :: FunMutDecl () () -> CGen (FunMutDecl UType UScheme, Subst)
checkFunMutDecl (SingleDecl funDecl) =
  checkFunDecl funDecl >>= \(fd, s) -> do
    fd' <- generaliseFunScheme fd
    pure (SingleDecl fd',s)

checkFunMutDecl (MutualDecls loc funDecls) = do
  -- Generate schemes with placeholder uvars for all functions in block
  forM_ funDecls $ \(FunDecl _ name params _ _ _ _) -> do
    (paramUVars,retUVar) <- genFunDeclTypes name params
    envGlobalInsertFun name $ UScheme S.empty $ Fun paramUVars retUVar
  -- Check fun decls in order without generalising
  (fds,ss) <- checkList checkFunDecl funDecls
  -- Substitute and generalise all function schemes
  let fds' = map (annotateFunDecl ss) fds
  -- let fds' = map (subst ss) fds
  fds'' <- forM fds' generaliseFunScheme
  pure (MutualDecls loc fds'', ss)

generaliseFunScheme :: FunDecl UType UScheme -> CGen (FunDecl UType UScheme)
generaliseFunScheme (FunDecl loc name params mTy varDecls stmts (UScheme _ funTy)) = do
  -- Generalise over free uvars
  let binders = freeUVars funTy
  let funScheme = UScheme binders funTy
  -- Update function scheme in environment
  envGlobalInsertFun name funScheme
  pure $ FunDecl loc name params mTy varDecls stmts funScheme

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

checkFunDecl :: FunDecl () () -> CGen (FunDecl UType UScheme, Subst)
checkFunDecl (FunDecl loc name params mTy varDecls stmts _) = do
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
  let s = stmtsSubst <> varDeclsSubst
  let funTy' = subst s funTy
  tySubst <- case mTy of
    Nothing -> pure mempty
    Just userTy -> do -- Substitute uvars for tvars in annotated type
      userTy' <- instantiateUserType userTy
      unify funTy' userTy' loc
  let funScheme = UScheme S.empty (subst tySubst funTy')
  envGlobalInsertFun name funScheme
  pure (FunDecl loc name params mTy varDecls' stmts' funScheme, tySubst <> s)

instantiateUserType :: UType -> CGen UType
instantiateUserType ty = do
  let tVars = freeTVars ty
  s <- sequence $ M.fromSet (const freshVar) tVars
  pure $ substTVars s ty


checkStmts :: [Stmt ()] -> CGen ([Stmt UType], Subst)
checkStmts = checkList checkStmt

checkStmt :: Stmt () -> CGen (Stmt UType, Subst)
checkStmt (If loc condExpr thenStmts elseStmts) = do
  (condExpr', condExprType, condExprSubst) <- checkExpr condExpr
  s <- unify condExprType Bool loc
  applySubst s
  (thenStmts', thenStmtsSubst) <- checkStmts thenStmts
  (elseStmts', elseStmtsSubst) <- checkStmts elseStmts
  pure (If loc condExpr' thenStmts' elseStmts',
    elseStmtsSubst <> thenStmtsSubst <> s <> condExprSubst)

checkStmt (While loc condExpr loopStmts) = do
  (condExpr', condExprType, condExprSubst) <- checkExpr condExpr
  s <- unify condExprType Bool loc
  applySubst s
  (loopStmts', loopStmtsSubst) <- checkStmts loopStmts
  pure (While loc condExpr' loopStmts', loopStmtsSubst <> s <> condExprSubst)

checkStmt (Assign loc varLookup _ expr) = do
  (expr', exprType, exprSubst) <- checkExpr expr
  (varTy,s) <- go varLookup exprType
  pure (Assign loc varLookup varTy expr', s <> exprSubst)
  where
    go :: VarLookup -> UType -> CGen (UType, Subst)
    go (VarId loc' name) exprType = do
      mVarType <- envLookupVar name
      case mVarType of
        Nothing -> throwLocError loc' $ "No variable declaration matching `" <> name <> "`"
        Just varType -> do
          s <- unify exprType varType loc
          applySubst s
          pure (varType,s)
    go (VarField loc' varLkp field) exprType = do
      case field of
        Head -> go varLkp (List exprType)
        Tail -> go varLkp exprType
        Fst -> do
          uVarSnd <- UVar <$> freshVar
          go varLkp (Prod exprType uVarSnd)
        Snd -> do
          uVarFst <- UVar <$> freshVar
          go varLkp (Prod uVarFst exprType)
        (SelField name) -> do
          (inTy,outTy) <- envLookupSelector name >>= \case
            Nothing -> throwLocError loc' $ "Could not find selector `" <> name <> "`"
            Just (UScheme _ (Fun [dataTy@(Data _ _)] outTy)) -> pure (dataTy,outTy)
            Just _ -> error $ "Selector `" <> T.unpack name <> "` does not map out of data type!"
          s <- unify exprType outTy loc'
          (varTy,s') <- go varLkp inTy
          pure (varTy, s <> s')

checkStmt (FunCall loc funName args) = do
  (args',_,s) <- checkFunCall funName args loc
  pure (FunCall loc funName args', s)

checkStmt (Return loc mExpr) = do
  envLookupRetType >>= \case
    Nothing -> error "RetType not found in environment!"
    Just retType -> do
      case mExpr of
        Nothing -> do
          s <- unify retType Void loc
          applySubst s
          pure (Return loc Nothing, s)
        Just expr -> do
          (expr', exprType, exprSubst) <- checkExpr expr
          s <- unify retType exprType loc
          applySubst s
          pure (Return loc (Just expr'), s <> exprSubst)


checkExpr :: Expr () -> CGen (Expr UType, UType, Subst)
checkExpr (Ident loc name _) = do
  -- TODO: We need a way to check whether this a local or global variable
  envLookupVar name >>= \case
      Nothing -> throwLocError loc $ "Could not find variable `" <> name <> "`"
      Just ut -> pure (Ident loc name ut, ut, mempty)
checkExpr (TA.Int loc n _) = pure (TA.Int loc n Int, Int, mempty)
checkExpr (TA.Char loc c _) = pure (TA.Char loc c Char, Char, mempty)
checkExpr (TA.Bool loc b _) = pure (TA.Bool loc b Bool, Bool, mempty)
checkExpr (FunCallE loc funName args _) = do
  (args', retType, s) <- checkFunCall funName args loc
  pure (FunCallE loc funName args' retType, retType, s)
checkExpr (EmptyList loc _) = do
  ty <- List . UVar <$> freshVar
  pure (EmptyList loc ty, ty, mempty)
checkExpr (Tuple loc e1 e2 _) = do
  (e1',t1,s1) <- checkExpr e1
  (e2',t2,s2) <- checkExpr e2
  let ty = Prod t1 t2
  pure (Tuple loc e1' e2' ty, ty, s2 <> s1)

checkExprs :: [Expr ()] -> CGen ([Expr UType], [UType], Subst)
checkExprs [] = pure ([],[],mempty)
checkExprs (expr:exprs) = do
  (expr', exprType, exprSubst) <- checkExpr expr
  (exprs', exprsTypes, exprsSubst) <- checkExprs exprs
  pure (expr':exprs', exprType:exprsTypes, exprSubst <> exprsSubst)

checkFunCall :: FunName -> [Expr ()] -> Loc -> CGen ([Expr UType], UType, Subst)
checkFunCall funName args loc = do
  funType <- getFunType funName loc >>= instantiateScheme
  case funType of
    Fun paramTypes retType -> do
      (args', argsTypes, argsSubst) <- checkExprs args
      if length paramTypes /= length argsTypes then throwLocError loc $
        "Incorrect number of arguments in call to " <> pack (show funName)
      else do
        s <- unifyLists paramTypes argsTypes loc
        applySubst s
        pure (args', subst s retType, s <> argsSubst)
    _ -> error $ -- Invalid global env entry, internal error
      "Function " <> show funName <> " does not have function type"
  -- where
  --   unifyLists :: [UType] -> [UType] -> CGen Subst
  --   unifyLists [] [] = pure mempty
  --   unifyLists (ty1:tys1) (ty2:tys2) = do
  --     s <- unify ty1 ty2 loc
  --     ss <- unifyLists (subst s <$> tys1) (subst s <$> tys2)
  --     pure $ ss <> s
  --   unifyLists _ _ =
  --     throwLocError loc $ "Incorrect number of arguments in call to " <> pack (show funName)

instantiateScheme :: UScheme -> CGen UType
instantiateScheme (UScheme tVars ty) = do
  s <- sequence $ M.fromSet (const $ UVar <$> freshVar) tVars
  pure $ subst (Subst s) ty

getFunType :: FunName -> Loc -> CGen UScheme
getFunType (Name name) loc = do
    mFunType <- envLookupFun name
    case mFunType of
      Nothing -> throwLocError loc $ "No declaration for function `" <> name <> "`"
      Just funType -> pure funType
getFunType (CtorCall name) loc = envLookupCtor name >>= \case
    Just ctorTy -> pure ctorTy
    Nothing -> throwLocError loc $ "No declaration for constructor `" <> name <> "`"
getFunType (Selector name) loc = envLookupSelector name >>= \case
    Just selTy -> pure selTy
    Nothing -> throwLocError loc $ "No declaration for selector `" <> name <> "`"
getFunType Not     _ = pure $ UScheme S.empty (Fun [Bool] Bool)
getFunType Neg     _ = pure $ UScheme S.empty (Fun [Int] Int)
getFunType Add     _ = pure $ UScheme S.empty (Fun [Int, Int] Int)
getFunType Sub     _ = pure $ UScheme S.empty (Fun [Int, Int] Int)
getFunType Mul     _ = pure $ UScheme S.empty (Fun [Int, Int] Int)
getFunType Div     _ = pure $ UScheme S.empty (Fun [Int, Int] Int)
getFunType Mod     _ = pure $ UScheme S.empty (Fun [Int, Int] Int)
getFunType Eq      _ = pure $ UScheme (S.singleton 0) (Fun [UVar 0, UVar 0] Bool) -- TODO: We probably want an Eq type class for this
getFunType Neq     _ = pure $ UScheme (S.singleton 0) (Fun [UVar 0, UVar 0] Bool) -- Same here
getFunType Lt      _ = pure $ UScheme S.empty (Fun [Int,Int] Bool)
getFunType Gt      _ = pure $ UScheme S.empty (Fun [Int,Int] Bool)
getFunType Lte     _ = pure $ UScheme S.empty (Fun [Int,Int] Bool)
getFunType Gte     _ = pure $ UScheme S.empty (Fun [Int,Int] Bool)
getFunType And     _ = pure $ UScheme S.empty (Fun [Bool,Bool] Bool)
getFunType Or      _ = pure $ UScheme S.empty (Fun [Bool,Bool] Bool)
getFunType Cons    _ = pure $ UScheme (S.singleton 0) (Fun [UVar 0, List (UVar 0)] (List (UVar 0)))
getFunType IsEmpty _ = pure $ UScheme (S.singleton 0) (Fun [List (UVar 0)] Bool)
getFunType Print   _ = pure $ UScheme (S.singleton 0) (Fun [UVar 0] Void)
getFunType HeadFun _ = pure $ UScheme (S.singleton 0) (Fun [List $ UVar 0] $ UVar 0)
getFunType TailFun _ = pure $ UScheme (S.singleton 0) (Fun [List $ UVar 0] (List (UVar 0)))
getFunType FstFun  _ = pure $ UScheme (S.fromList [0, 1]) (Fun [Prod (UVar 0) (UVar 1)] (UVar 0))
getFunType SndFun  _ = pure $ UScheme (S.fromList [0, 1]) (Fun [Prod (UVar 0) (UVar 1)] (UVar 1))
