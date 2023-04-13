{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module TypeInference.ConstraintGen where

import TypeInference.Definition
import TypeInference.Unify
import qualified Syntax.TypeAST as T
import Control.Monad.State
import Control.Monad.Except ( MonadError(throwError) )
import Data.Text (pack)
import Control.Monad.RWS (MonadReader(local, ask))
import Data.Maybe (fromJust)

checkProgram :: T.Program () -> CGen (T.Program UType, Subst)
checkProgram (T.Program varDecls funDecls) = do
  (vds,s1) <- checkVarDecls varDecls True
  (fds,s2) <- checkFunDecls funDecls
  pure (T.Program vds fds, s1 `compose` s2)

checkVarDecls :: [T.VarDecl ()] -> Bool -> CGen ([T.VarDecl UType], Subst)
checkVarDecls [] _ = pure ([], emptySubst)
checkVarDecls (varDecl:varDecls) isTopLevel = do
  (varDecl',s) <- checkVarDecl varDecl isTopLevel
  (varDecls',ss) <- checkVarDecls varDecls isTopLevel
  pure (varDecl':varDecls', ss `compose` s)

checkVarDecl :: T.VarDecl () -> Bool -> CGen (T.VarDecl UType, Subst)
checkVarDecl (T.VarDecl mTy name expr _) isTopLevel = do
  uVar <- UVar <$> freshVar
  (expr', exprType, exprSubst) <- checkExpr expr
  if isTopLevel
    then modifyGlobalEnv $ envInsert (TermVar name) uVar
    else modifyLocalEnv $ envInsert (TermVar name) uVar
  s <- unify exprType uVar
  {- TODO: Check inferred type against user-specified type
  case mTy of
    Just ty -> do
      sAnnot <- unify ty uVar
      ...
    Nothing -> pure (T.VarDecl mTy name expr' uVar, exprSubst)
  -}
  when isTopLevel $
    modifyGlobalEnv $ envMap (subst s)
  modifyLocalEnv $ envMap (subst s)
  pure (T.VarDecl mTy name expr' uVar, s `compose` exprSubst)

checkFunDecls :: [T.FunDecl ()] -> CGen ([T.FunDecl UType], Subst)
checkFunDecls [] = pure ([], emptySubst)
checkFunDecls (funDecl:funDecls) = do
  (funDecl',s) <- checkFunDecl funDecl
  (funDecls',ss) <- checkFunDecls funDecls
  pure (funDecl':funDecls', ss `compose` s)

checkFunDecl :: T.FunDecl () -> CGen (T.FunDecl UType, Subst)
checkFunDecl (T.FunDecl name params mTy varDecls stmts _) = do
  uVarsParams <- forM params (const $ UVar <$> freshVar)
  uVarRet <- freshVar
  local (const $ Just uVarRet) $ do
    modifyLocalEnv (\env -> foldr (uncurry envInsert) env (zip (TermVar <$> params) uVarsParams))
    -- Insert mapping for function name in order to typecheck recursive calls
    modifyLocalEnv $ envInsert (FunName name) (Fun uVarsParams (UVar uVarRet))
    (varDecls',varDeclsSubst) <- checkVarDecls varDecls False
    (stmts',stmtsSubst) <- checkStmts stmts
    modifyLocalEnv $ const emptyEnv
    -- TODO: Check user-specified type against inferred type, insert in env
    modifyGlobalEnv $ envInsert (FunName name) (Fun uVarsParams (UVar uVarRet))
    let s = stmtsSubst `compose` varDeclsSubst
    modifyGlobalEnv $ envMap (subst s)
    pure (T.FunDecl name params mTy varDecls' stmts' (Fun uVarsParams (UVar uVarRet)), s)

checkStmts :: [T.Stmt ()] -> CGen ([T.Stmt UType], Subst)
checkStmts [] = pure ([], emptySubst)
checkStmts (stmt:stmts) = do
  (stmt',s) <- checkStmt stmt
  (stmts',ss) <- checkStmts stmts
  pure (stmt':stmts', ss `compose` s)

checkStmt :: T.Stmt () -> CGen (T.Stmt UType, Subst)
checkStmt (T.If condExpr thenStmts elseStmts) = do
  (condExpr', condExprType, condExprSubst) <- checkExpr condExpr
  case condExprType of
    Bool -> throwError "`if` condition must have type Bool"
    _ -> do
      (thenStmts', thenStmtsSubst) <- checkStmts thenStmts
      (elseStmts', elseStmtsSubst) <- checkStmts elseStmts
      pure (T.If condExpr' thenStmts' elseStmts',
        elseStmtsSubst `compose` thenStmtsSubst `compose` condExprSubst)

checkStmt (T.While condExpr loopStmts) = do
  (condExpr', condExprType, condExprSubst) <- checkExpr condExpr
  case condExprType of
    Bool -> throwError "`while` condition must have type Bool"
    _ -> do
      (loopStmts', loopStmtsSubst) <- checkStmts loopStmts
      pure (T.While condExpr' loopStmts', loopStmtsSubst `compose` condExprSubst)

checkStmt (T.Assign varLookup expr) = do
  (expr', exprType, exprSubst) <- checkExpr expr
  s <- foo varLookup exprType
  pure (T.Assign varLookup expr', s `compose` exprSubst)
  where
    foo :: T.VarLookup -> UType -> CGen Subst
    foo (T.VarId name) exprType = do
      mVarType <- lookupEnv (TermVar name)
      case mVarType of
        Nothing -> throwError $ "No variable declaration matching " <> name
        Just varType -> unify exprType varType
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
  uVarRet <- UVar . fromJust <$> ask
  case mExpr of
    Nothing -> do
      s <- unify uVarRet Void
      pure (T.Return Nothing, s)
    Just expr -> do
      (expr', exprType, exprSubst) <- checkExpr expr
      s <- unify uVarRet exprType
      pure (T.Return (Just expr'), s `compose` exprSubst)


checkExpr :: T.Expr () -> CGen (T.Expr UType, UType, Subst)
checkExpr (T.Ident name _) = do
  -- TODO: We need a way to check whether this a local or global variable
  mut <- lookupEnv (TermVar name)
  case mut of
    Nothing -> error ""
    Just ut -> pure (T.Ident name ut, ut, emptySubst)
checkExpr (T.Int n _) = pure (T.Int n Int, Int, emptySubst)
checkExpr (T.Char c _) = pure (T.Char c Char, Char, emptySubst)
checkExpr (T.Bool b _) = pure (T.Bool b Bool, Bool, emptySubst)
checkExpr (T.FunCallE funName args _) = do
  (args', retType, s) <- checkFunCall funName args
  pure (T.FunCallE funName args' retType, retType, s)
checkExpr (T.EmptyList _) = pure (T.EmptyList ty, ty, emptySubst)
  where ty = List Int -- TODO: Should be ∀a.[a]
checkExpr (T.Tuple e1 e2 _) = do
  (e1',t1,s1) <- checkExpr e1
  (e2',t2,s2) <- checkExpr e2
  let ty = Prod t1 t2
  -- TODO: How to extract the types from e1' and e2' here?
  pure (T.Tuple e1' e2' ty, ty, s1 `compose` s2)

checkExprs :: [T.Expr ()] -> CGen ([T.Expr UType], [UType], Subst)
checkExprs [] = pure ([],[],emptySubst)
checkExprs (expr:exprs) = do
  (expr', exprType, exprSubst) <- checkExpr expr
  (exprs', exprsTypes, exprsSubst) <- checkExprs exprs
  pure (expr':exprs', exprType:exprsTypes, exprSubst `compose` exprsSubst)



checkFunCall :: T.FunName -> [T.Expr ()] -> CGen ([T.Expr UType], UType, Subst)
checkFunCall funName args = do
  funType <- getFunType funName
  case funType of
    UScheme tVars (Fun paramTypes retType) -> do
      (args', argsTypes, argsSubst) <- checkExprs args
      s <- unifyLists paramTypes argsTypes
      pure (args', retType, s `compose` argsSubst)
    _ -> throwError $ "Function " <> pack (show funName) <> " does not have a function type"
  where
    unifyLists :: [UType] -> [UType] -> CGen Subst
    unifyLists [] [] = pure emptySubst
    unifyLists (ty1:tys1) (ty2:tys2) = do
      s <- unify ty1 ty2
      ss <- unifyLists tys1 tys2
      pure $ ss `compose` s
    unifyLists _ _ =
      throwError $ "Incorrect number of arguments in call to " <> pack (show funName)


getFunType :: T.FunName -> CGen UScheme
getFunType funName = do
  case funName of
    T.Name name -> do
      mFunType <- lookupEnv (FunName name)
      case mFunType of
        Nothing -> throwError $ "No declaration for function " <> name
        Just funType -> pure $ UScheme [] funType
    T.Not -> pure $ UScheme [] (Fun [Bool] Bool)
    T.Neg -> pure $ UScheme [] (Fun [Int] Int)
    T.Add -> pure $ UScheme [] (Fun [Int] Int)
    T.Sub -> pure $ UScheme [] (Fun [Int] Int)
    T.Mul -> pure $ UScheme [] (Fun [Int] Int)
    T.Div -> pure $ UScheme [] (Fun [Int] Int)
    T.Mod -> pure $ UScheme [] (Fun [Int] Int)
    T.Eq -> pure $ UScheme ["a"] (Fun [TVar "a", TVar "a"] (TVar "a")) -- TODO: We probably want an Eq type class for this
    T.Neq -> pure $ UScheme ["a"] (Fun [TVar "a", TVar "a"] (TVar "a")) -- Same here
    T.Lt -> pure $ UScheme [] (Fun [Int,Int] Int)
    T.Gt -> pure $ UScheme [] (Fun [Int,Int] Int)
    T.Lte -> pure $ UScheme [] (Fun [Int,Int] Int)
    T.Gte -> pure $ UScheme [] (Fun [Int,Int] Int)
    T.And -> pure $ UScheme [] (Fun [Bool,Bool] Bool)
    T.Or -> pure $ UScheme [] (Fun [Bool,Bool] Bool)
    T.Cons -> pure $ UScheme ["a"] (Fun [TVar "a", List (TVar "a")] (List (TVar "a")))
    T.IsEmpty -> pure $ UScheme ["a"] (Fun [List (TVar "a")] Bool)
    T.Print -> pure $ UScheme ["a"] (Fun [TVar "a"] Void)
    T.HeadFun -> pure $ UScheme ["a"] (Fun [List $ TVar "a"] $ TVar "a")
    T.TailFun -> pure $ UScheme ["a"] (Fun [List $ TVar "a"] (List (TVar "a")))
    T.FstFun -> pure $ UScheme ["a","b"] (Fun [Prod (TVar "a") (TVar "b")] (TVar "a"))
    T.SndFun -> pure $ UScheme ["a","b"] (Fun [Prod (TVar "a") (TVar "b")] (TVar "b"))
