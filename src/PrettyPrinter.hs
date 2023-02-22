module PrettyPrinter (prettyPrinter) where

import Control.Monad (unless)
import Data.List (intersperse)
import qualified Data.Text.IO as T
import Syntax.Program (Expr, Field (..), FunDecl (FunDecl), Program (Program), Stmt (..), VarDecl (..))
import qualified Syntax.Types as Ty

-- Represents the amount of spaces to indent after a newline
type Indenation = Int

tabWidth :: Indenation
tabWidth = 4

printIndentation :: Indenation -> IO ()
printIndentation i = putStr (replicate i ' ')

sepBy :: String -> (a -> IO ()) -> [a] -> IO ()
sepBy sep f xs =
  sequence_ $ intersperse (putStr sep) $ f <$> xs

prettyPrintProgram :: Indenation -> Program -> IO ()
prettyPrintProgram _ (Program varDecls funDecls) = do
  sepBy "\n" (prettyPrintVarDecl 0) varDecls
  unless (null varDecls || null funDecls) $ putStrLn ""
  sepBy "\n\n" prettyPrintFunDecl funDecls
  putStrLn ""

prettyPrintVarDecl :: Indenation -> VarDecl -> IO ()
prettyPrintVarDecl i (VarDecl typeM ident e) = do
  printIndentation i
  case typeM of
    Nothing -> putStr "var"
    Just typ -> prettyPrintType typ
  putChar ' '
  T.putStr ident
  putStr " = "
  prettyPrintExpr e
  putChar ';'

prettyPrintType :: Ty.Type -> IO ()
prettyPrintType Ty.IntT = putStr "Int"
prettyPrintType Ty.BoolT = putStr "Bool"
prettyPrintType Ty.CharT = putStr "Char"
prettyPrintType Ty.Void = putStr "Void"
prettyPrintType (Ty.TyVar ident) = T.putStr ident
prettyPrintType (Ty.Prod t1 t2) = do
  putChar '('
  prettyPrintType t1
  putStr ", "
  prettyPrintType t2
  putChar ')'
prettyPrintType (Ty.List t) = do
  putChar '['
  prettyPrintType t
  putChar ']'
prettyPrintType (Ty.Fun argTypes retType) = do
  sepBy " " prettyPrintType argTypes
  putStr " -> "
  prettyPrintType retType

prettyPrintExpr :: Expr -> IO ()
prettyPrintExpr e = putStr "TODO: Expression printing with right parens"

prettyPrintFunDecl :: FunDecl -> IO ()
prettyPrintFunDecl (FunDecl funName argNames retTypeM varDecls stmts) = do
  T.putStr funName
  putChar '('
  sepBy ", " T.putStr argNames
  putChar ')'
  case retTypeM of
    Nothing -> pure ()
    Just retType -> do
      putStr " :: "
      prettyPrintType retType
  putStrLn " {"
  sepBy "\n" (prettyPrintVarDecl 4) varDecls
  unless (null varDecls || null stmts) $ putStrLn ""
  sepBy "\n" (prettyPrintStmt 4) stmts
  putStrLn ""
  putChar '}'

prettyPrintStmt :: Indenation -> Stmt -> IO ()
prettyPrintStmt i (If e stmts1 stmts2) = do
  printIndentation i
  putStr "if ("
  prettyPrintExpr e
  putStrLn ") {"
  sepBy "\n" (prettyPrintStmt (i + tabWidth)) stmts1
  putStrLn ""
  printIndentation i
  putStr "}"
  unless (null stmts2) $ do
    putStrLn " else {"
    sepBy "\n" (prettyPrintStmt (i + tabWidth)) stmts2
    putStrLn ""
    printIndentation i
    putStr "}"
prettyPrintStmt i (While e stmts) = do
  printIndentation i
  putStrLn "while ("
  prettyPrintExpr e
  putStr ") {"
  sepBy "\n" (prettyPrintStmt (i + tabWidth)) stmts
  putStrLn ""
  printIndentation i
  putStr "}"
prettyPrintStmt i (Assign field e) = do
  printIndentation i
  prettyPrintField field
  putStr " := "
  prettyPrintExpr e
  putStr ";"
prettyPrintStmt i (FunCall funName args) = do
  printIndentation i
  T.putStr funName
  putChar '('
  sepBy ", " prettyPrintExpr args
  putStr ");"
prettyPrintStmt i (Return eM) = do
  printIndentation i
  putStr "return"
  case eM of
    Nothing -> pure ()
    Just e -> putChar ' ' >> prettyPrintExpr e
  putChar ';'

prettyPrintField :: Field -> IO ()
prettyPrintField (Ident ident) = T.putStr ident
prettyPrintField (Head field) = prettyPrintField field >> putStr ".hd"
prettyPrintField (Tail field) = prettyPrintField field >> putStr ".tl"
prettyPrintField (Fst field) = prettyPrintField field >> putStr ".fst"
prettyPrintField (Snd field) = prettyPrintField field >> putStr ".snd"

prettyPrinter :: Program -> IO ()
prettyPrinter = prettyPrintProgram 0
