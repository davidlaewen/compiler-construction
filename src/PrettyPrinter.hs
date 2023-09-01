module PrettyPrinter (prettyPrinter) where

import Data.List (intersperse)
import qualified Data.Text.IO as T
import Syntax.ParseAST
import Control.Monad (unless)
import Parser.Tokens (Keyword(..), Token(..), Symbol (..))

-- Represents the amount of spaces to indent after a newline
type Indentation = Int

tabWidth :: Indentation
tabWidth = 4

printIndentation :: Indentation -> IO ()
printIndentation i = putStr (replicate i ' ')

sepBy :: String -> (a -> IO ()) -> [a] -> IO ()
sepBy sep f xs =
  sequence_ $ intersperse (putStr sep) $ f <$> xs

prettyPrintProgram :: Indentation -> Program -> IO ()
prettyPrintProgram _ (Program varDecls funDecls) = do
  sepBy "\n" (prettyPrintVarDecl 0) varDecls
  unless (null varDecls || null funDecls) $ putStrLn ""
  sepBy "\n\n" prettyPrintFunMutDecl funDecls
  putStrLn ""

prettyPrintVarDecl :: Indentation -> VarDecl -> IO ()
prettyPrintVarDecl i (VarDecl typeM ident e) = do
  printIndentation i
  case typeM of
    Nothing -> putStr $ show KwVar
    Just typ -> prettyPrintType typ
  putChar ' '
  T.putStr ident
  putStr " = "
  prettyPrintExpr e
  putStr $ show SymSemicolon

prettyPrintType :: Type -> IO ()
prettyPrintType IntT = putStr $ show KwInt
prettyPrintType BoolT = putStr $ show KwBool
prettyPrintType CharT = putStr $ show KwChar
prettyPrintType Void = putStr $ show KwVoid
prettyPrintType (TyVar ident) = T.putStr ident
prettyPrintType (Prod t1 t2) = do
  putStr $ show SymParenLeft
  prettyPrintType t1
  putStr ", "
  prettyPrintType t2
  putStr $ show SymParenRight
prettyPrintType (List t) = do
  putStr $ show SymBracketLeft
  prettyPrintType t
  putStr $ show SymBracketRight
prettyPrintType (Fun argTypes retType) = do
  sepBy " " prettyPrintType argTypes
  putStr " -> "
  prettyPrintType retType
prettyPrintType GarbageT = putStr "Garbage"

prettyPrintExpr :: Expr -> IO ()
prettyPrintExpr = go 0
  where
    go :: Int -> Expr -> IO ()
    go _ (Ident t) = T.putStr t
    go _ (ExprLookup exprLookup) = prettyPrintExprLookup exprLookup
    go _ (Int n) = putStr $ show n
    go _ (Char c) = putChar '\'' >> putChar c >> putChar '\''
    go _ (Bool b) = putStr $ show (BoolLit b)
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
    prettyPrintBinOp Lte  = putStr " <= "
    prettyPrintBinOp Gte  = putStr " >= "
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
    precedence Lte  = 2
    precedence Gte  = 2

    precedence Cons = 3

    precedence Add  = 4
    precedence Sub  = 4

    precedence Mul  = 5
    precedence Div  = 5
    precedence Mod  = 5

    unOpPrecedence :: Int
    unOpPrecedence = 6

prettyPrintFunMutDecl :: FunMutDecl -> IO ()
prettyPrintFunMutDecl (SingleDecl funDecl) = prettyPrintFunDecl 0 funDecl
prettyPrintFunMutDecl (MutualDecls funDecls) = do
  putStrLn "mutual {"
  sepBy "\n" (prettyPrintFunDecl 4) funDecls
  putChar '\n'
  putChar '}'

prettyPrintFunDecl :: Indentation -> FunDecl -> IO ()
prettyPrintFunDecl i (FunDecl funName argNames retTypeM varDecls stmts) = do
  printIndentation i
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
  sepBy "\n" (prettyPrintVarDecl $ i + tabWidth) varDecls
  unless (null varDecls || null stmts) $ putStrLn ""
  sepBy "\n" (prettyPrintStmt $ i + tabWidth) stmts
  putStrLn ""
  printIndentation i
  putChar '}'

prettyPrintStmt :: Indentation -> Stmt -> IO ()
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
prettyPrintStmt i (Assign varLookup e) = do
  printIndentation i
  prettyPrintVarLookup varLookup
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

prettyPrintVarLookup :: VarLookup -> IO ()
prettyPrintVarLookup (VarId t) = T.putStr t
prettyPrintVarLookup (VarField varLookup field) =
  prettyPrintVarLookup varLookup >> prettyPrintField field

prettyPrintExprLookup :: ExprLookup -> IO ()
prettyPrintExprLookup (ExprField expr field) =
  prettyPrintExpr expr >> prettyPrintField field

prettyPrintField :: Field -> IO ()
prettyPrintField f = putStr $ show SymDot ++ show (field2Kw f)
  where
    field2Kw Head = KwHead
    field2Kw Tail = KwTail
    field2Kw Fst = KwFst
    field2Kw Snd = KwSnd


prettyPrinter :: Program -> IO ()
prettyPrinter = prettyPrintProgram 0
