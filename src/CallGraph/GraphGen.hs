module CallGraph.GenGraph (programToGraph) where

import CallGraph.Definition
import Syntax.TypeAST

import Data.Graph


-- | Takes vertex for current decl and expression, adds edges for fun calls
exprToGraph :: Vertex -> Expr () -> GraphGen ()
exprToGraph declVtx (FunCallE _ funName args _) = do
  case funName of -- Insert edge for function call
    Name name -> insertEdge declVtx name
    _ -> pure () -- Unless it's a built-in function
  mapM_ (exprToGraph declVtx) args
-- Check for function calls in left and right subterm
exprToGraph declVtx (Tuple _ lhs rhs _) = do
  exprToGraph declVtx lhs
  exprToGraph declVtx rhs
-- Trivial cases; nothing happens here
exprToGraph _ Ident{} = pure ()
exprToGraph _ Int{} = pure ()
exprToGraph _ Bool{} = pure ()
exprToGraph _ Char{} = pure ()
exprToGraph _ EmptyList{} = pure ()

stmtToGraph :: Vertex -> Stmt () -> GraphGen ()
stmtToGraph declVtx (If loc condExpr thenStmts elseStmts) = do
  exprToGraph declVtx condExpr
  mapM_ (stmtToGraph declVtx) thenStmts
  mapM_ (stmtToGraph declVtx) elseStmts

stmtToGraph declVtx (While loc condExpr stmts) = do
  exprToGraph declVtx condExpr
  mapM_ (stmtToGraph declVtx) stmts

stmtToGraph declVtx (Assign _ varLookup ty expr) =
  exprToGraph declVtx expr

stmtToGraph declVtx (FunCall loc funName args) = do
  case funName of -- Again, insert edge only for user-defined functions
    Name name -> insertEdge declVtx name
    _ -> pure ()
  mapM_ (exprToGraph declVtx) args

stmtToGraph declVtx (Return _ mExpr) = do
  mapM_ (exprToGraph declVtx) mExpr

-- | Only to be called for var decls at the start of a function decl, not the
-- global var declarations, which we don't include in the SCC analysis
varDeclToGraph :: Vertex -> VarDecl () -> GraphGen ()
varDeclToGraph declVtx (VarDecl loc mty varName expr ty) = do
  exprToGraph declVtx expr

funDeclToGraph :: FunDecl () () -> GraphGen ()
funDeclToGraph funDecl@(FunDecl _ funName argNames mty varDecls stmts ty) = do
  declVtx <- nameToVertex funName -- Get existing or new vertex ID
  insertDecl declVtx funDecl -- Insert into fun decl map
  mapM_ (varDeclToGraph declVtx) varDecls
  mapM_ (stmtToGraph declVtx) stmts

funMutDeclToGraph :: FunMutDecl () () -> GraphGen ()
funMutDeclToGraph (SingleDecl funDecl) = funDeclToGraph funDecl
-- For now, we just ignore the existing grouping into `mutual` blocks
funMutDeclToGraph (MutualDecls _ funDecls) = do
  mapM_ funDeclToGraph funDecls

programToGraph :: Program () () -> GraphGen ()
programToGraph (Program varDecls funMutDecls) = do
  mapM_ funMutDeclToGraph funMutDecls
