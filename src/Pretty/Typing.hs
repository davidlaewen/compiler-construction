module Pretty.Typing (printProgram) where

import qualified Data.Text.IO as T
import Syntax.TypeAST as AST
import Parser.Tokens (Keyword(..), Token(..), Symbol (..))
import Pretty.Common
import Control.Monad (unless)
import TypeInference.Types as Ty


printProgram :: (Show va, Show fa) => Program va fa -> IO ()
printProgram (Program varDecls funDecls) = do
  sepBy "\n" (printVarDecl 0) varDecls
  unless (null varDecls || null funDecls) $ putStrLn ""
  sepBy "\n\n" (printFunMutDecl 0) funDecls

printVarDecl :: Show a => Indentation -> VarDecl a -> IO ()
printVarDecl i (VarDecl _ typeM ident e ty) = do
  printIndentation i
  case typeM of
    Nothing -> putShow KwVar
    Just typ -> printUType typ
  spaces $ printTyLabel ty
  T.putStr ident
  spaces (putShow SymEq)
  printExpr e
  putShow SymSemicolon

printFunMutDecl :: (Show a, Show b) => Indentation -> FunMutDecl a b -> IO ()
printFunMutDecl i (SingleDecl funDecl) = printFunDecl i funDecl
printFunMutDecl i (MutualDecls _ funDecls) = do
  putShow KwMutual
  printBlock i $
    sepBy "\n" (printFunDecl $ i + tabWidth) funDecls

printFunDecl :: (Show a, Show b) => Indentation -> FunDecl a b -> IO ()
printFunDecl i (FunDecl _ funName argNames retTypeM varDecls stmts ty) = do
  printIndentation i
  T.putStr funName
  parens $ sepBy ", " T.putStr argNames
  case retTypeM of
    Nothing -> pure ()
    Just retType -> do
      spaces $ putShow SymColonColon
      printUType retType
  spaces $ printTyLabel ty
  printBlock i (do
    sepBy "\n" (printVarDecl $ i + tabWidth) varDecls
    unless (null varDecls || null stmts) $ putChar '\n'
    sepBy "\n" (printStmt $ i + tabWidth) stmts)

printStmt :: Show a => Indentation -> Stmt a -> IO ()
printStmt i (If _ e stmts1 stmts2) = do
  printIndentation i
  putShow KwIf
  spaces $ parens $ printExpr e
  printBlock i $ sepBy "\n" (printStmt (i + tabWidth)) stmts1
  unless (null stmts2) $ do
    spaces $ putShow KwElse
    printBlock i $ sepBy "\n" (printStmt (i + tabWidth)) stmts2
printStmt i (While _ e stmts) = do
  printIndentation i
  putShow KwWhile
  spaces $ parens $ printExpr e
  printBlock i $
    sepBy "\n" (printStmt (i + tabWidth)) stmts
printStmt i (Assign _ varLookup ty e) = do
  printIndentation i
  printVarLookup varLookup
  printTyLabel ty
  spaces $ putShow SymEq
  printExpr e
  putShow SymSemicolon
printStmt i (FunCall _ funName args) = do
  printIndentation i
  putShow funName
  parens $ sepBy ", " printExpr args
  putShow SymSemicolon
printStmt i (Return _ eM) = do
  printIndentation i
  putStr $ show KwReturn
  case eM of
    Nothing -> pure ()
    Just e -> putChar ' ' >> printExpr e
  putShow SymSemicolon

printExpr :: Show a => Expr a -> IO ()
printExpr (Ident _ t ty) = T.putStr t >> printTyLabel ty
printExpr (AST.Int _ n ty) = putShow n >> printTyLabel ty
printExpr (AST.Bool _ b ty) = putShow (BoolLit b) >> printTyLabel ty
printExpr (AST.Char _ c ty) = putShow (CharLit c) >> printTyLabel ty
printExpr (EmptyList _ ty) =
  putShow SymBracketLeft >> putShow SymBracketRight >> printTyLabel ty
printExpr (FunCallE _ name args ty) = do
  putShow name
  parens $ sepBy ", " printExpr args
  printTyLabel ty
printExpr (Tuple _ e1 e2 ty) =
  parens $ printExpr e1 >> putStr ", " >> printExpr e2 >> printTyLabel ty

printUType :: UType -> IO ()
printUType Ty.Int = putShow KwInt
printUType Ty.Bool = putShow KwBool
printUType Ty.Char = putShow KwChar
printUType Ty.Void = putShow KwVoid
printUType (Ty.Prod ty1 ty2) =
  parens $ printUType ty1 >> putChar ',' >> printUType ty2
printUType (Ty.List ty) = brackets $ printUType ty
printUType (Ty.Fun argTys retTy) = do
  sepBy " " printUType argTys
  spaces $ putShow SymRightArrow
  printUType retTy
printUType (Ty.UVar i) = putChar 'u' >> putShow i
printUType (Ty.TVar t) = T.putStr t

printTyLabel :: Show a => a -> IO ()
printTyLabel l = putChar '<' >> putShow l >> putChar '>'

printVarLookup :: VarLookup -> IO ()
printVarLookup (VarId _ t) = T.putStr t
printVarLookup (VarField _ varLookup field) =
  printVarLookup varLookup >> printField field

printField :: Field -> IO ()
printField f = putShow SymDot >> putStr (show $ field2Kw f)
  where
    field2Kw Head = KwHead
    field2Kw Tail = KwTail
    field2Kw Fst = KwFst
    field2Kw Snd = KwSnd
