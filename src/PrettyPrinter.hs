module PrettyPrinter (prettyPrinter) where

import Control.Monad (unless)
import Data.List (intersperse)
import qualified Data.Text.IO as T
import Syntax.Program (Expr (..), Field (..), FunDecl (FunDecl), Program (Program), Stmt (..), Type(..), VarDecl (..), BinaryOp (..), UnaryOp (..))

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

prettyPrintType :: Type -> IO ()
prettyPrintType IntT = putStr "Int"
prettyPrintType BoolT = putStr "Bool"
prettyPrintType CharT = putStr "Char"
prettyPrintType Void = putStr "Void"
prettyPrintType (TyVar ident) = T.putStr ident
prettyPrintType (Prod t1 t2) = do
  putChar '('
  prettyPrintType t1
  putStr ", "
  prettyPrintType t2
  putChar ')'
prettyPrintType (List t) = do
  putChar '['
  prettyPrintType t
  putChar ']'
prettyPrintType (Fun argTypes retType) = do
  sepBy " " prettyPrintType argTypes
  putStr " -> "
  prettyPrintType retType
prettyPrintType GarbageT = putStr "Garbage"

prettyPrintExpr :: Expr -> IO ()
prettyPrintExpr = go 0
  where
    go :: Int -> Expr -> IO ()
    go _ (Field f) = prettyPrintField f
    go _ (Int n) = putStr $ show n
    go _ (Char c) = putChar '\'' >> putChar c >> putChar '\''
    go _ (Bool True) = putStr "true"
    go _ (Bool False) = putStr "false"
    go _ EmptyList = putStr "[]"
    go _ (FunCallE name args) = do
      T.putStr name
      putChar '('
      sepBy ", " (go 0) args
      putChar ')'
    go _ (Tuple e1 e2) = do
      putChar '('
      go 0 e1
      putStr ", "
      go 0 e2
      putChar ')'
    go _ (UnOp op e) = do
      case op of
        Not -> putChar '!'
        Neg -> putChar '-'
      go unOpPrecedence e
    go currentPrecedence (BinOp op e1 e2) =
      if currentPrecedence > precedence op
        then putChar '(' >> go (precedence op) e1 >> prettyPrintBinOp op >> go (precedence op) e2 >> putChar ')'
        else go currentPrecedence e1 >> prettyPrintBinOp op >> go currentPrecedence e2

    prettyPrintBinOp :: BinaryOp -> IO ()
    prettyPrintBinOp And  = putStr " && "
    prettyPrintBinOp Or   = putStr " || "
    prettyPrintBinOp Eq   = putStr " == "
    prettyPrintBinOp Neq  = putStr " != "
    prettyPrintBinOp Lt   = putStr " < "
    prettyPrintBinOp Gt   = putStr " > "
    prettyPrintBinOp Lteq = putStr " <= "
    prettyPrintBinOp Gteq = putStr " >= "
    prettyPrintBinOp Cons = putStr " : "
    prettyPrintBinOp Add  = putStr " + "
    prettyPrintBinOp Sub  = putStr " - "
    prettyPrintBinOp Mul  = putStr " * "
    prettyPrintBinOp Div  = putStr " / "
    prettyPrintBinOp Mod  = putStr " % "

    precedence :: BinaryOp -> Int
    precedence Or   = 0
    precedence And  = 1

    precedence Eq   = 2
    precedence Neq  = 2
    precedence Lt   = 2
    precedence Gt   = 2
    precedence Lteq = 2
    precedence Gteq = 2

    precedence Cons = 3

    precedence Add  = 4
    precedence Sub  = 4

    precedence Mul  = 5
    precedence Div  = 5
    precedence Mod  = 5

    unOpPrecedence :: Int
    unOpPrecedence = 6

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
prettyPrintStmt i GarbageS = do
  printIndentation i
  putStr "Garbage;"

prettyPrintField :: Field -> IO ()
prettyPrintField (Ident ident) = T.putStr ident
prettyPrintField (Head field) = prettyPrintField field >> putStr ".hd"
prettyPrintField (Tail field) = prettyPrintField field >> putStr ".tl"
prettyPrintField (Fst field) = prettyPrintField field >> putStr ".fst"
prettyPrintField (Snd field) = prettyPrintField field >> putStr ".snd"

prettyPrinter :: Program -> IO ()
prettyPrinter = prettyPrintProgram 0
