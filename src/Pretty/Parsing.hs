module Pretty.Parsing (prettyPrinter) where

import qualified Data.Text.IO as T
import Syntax.ParseAST
import Control.Monad (unless)
import Parser.Tokens (Keyword(..), Token(..), Symbol (..))
import Pretty.Common


prettyPrintProgram :: Indentation -> Program -> IO ()
prettyPrintProgram _ (Program _ varDecls funDecls) = do
  sepBy "\n" (prettyPrintVarDecl 0) varDecls
  unless (null varDecls || null funDecls) $ putStrLn ""
  sepBy "\n\n" (prettyPrintFunMutDecl 0) funDecls
  putStrLn ""

prettyPrintVarDecl :: Indentation -> VarDecl -> IO ()
prettyPrintVarDecl i (VarDecl _ typeM ident e) = do
  printIndentation i
  case typeM of
    Nothing -> putShow KwVar
    Just typ -> prettyPrintType typ
  putChar ' '
  T.putStr ident
  spaces (putShow SymEq)
  prettyPrintExpr e
  putShow SymSemicolon

prettyPrintType :: Type -> IO ()
prettyPrintType (IntT  _) = putStr $ show KwInt
prettyPrintType (BoolT _) = putStr $ show KwBool
prettyPrintType (CharT _) = putStr $ show KwChar
prettyPrintType (Void  _) = putStr $ show KwVoid
prettyPrintType (TyVar _ ident) = T.putStr ident
prettyPrintType (Prod _ t1 t2) =
  parens $ prettyPrintType t1 >> putStr ", " >> prettyPrintType t2
prettyPrintType (List _ t) = brackets $ prettyPrintType t
prettyPrintType (Fun _ argTypes retType) = do
  sepBy " " prettyPrintType argTypes
  spaces (putShow SymRightArrow)
  prettyPrintType retType
prettyPrintType GarbageType = putShow GarbageType

prettyPrintExpr :: Expr -> IO ()
prettyPrintExpr = go 0
  where
    go :: Int -> Expr -> IO ()
    go _ (Ident _ t) = T.putStr t
    go _ (ExprLookup _ exprLookup) = prettyPrintExprLookup exprLookup
    go _ (Int _ n) = putShow n
    go _ (Char _ c) = putShow (CharLit c)
    go _ (Bool _ b) = putShow (BoolLit b)
    go _ (EmptyList _) = putShow SymBracketLeft >> putShow SymBracketRight
    go _ (FunCallE _ name args) = do
      T.putStr name
      parens $ sepBy ", " (go 0) args
    go _ (Tuple _ e1 e2) = do
      parens $ go 0 e1 >> putStr ", " >> go 0 e2
    go _ (UnOp _ op e) = do
      putStr $ show op
      go unOpPrecedence e
    go currentPrecedence (BinOp _ op e1 e2) =
      if currentPrecedence > precedence op
        then parens $ go (precedence op) e1 >> prettyPrintBinOp op >> go (precedence op) e2
        else go currentPrecedence e1 >> prettyPrintBinOp op >> go currentPrecedence e2
    go _ GarbageExpr = putShow GarbageExpr

    prettyPrintBinOp :: BinaryOp -> IO ()
    prettyPrintBinOp binop = spaces (putStr $ show binop)

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

prettyPrintFunMutDecl :: Indentation -> FunMutDecl -> IO ()
prettyPrintFunMutDecl i (SingleDecl funDecl) = prettyPrintFunDecl i funDecl
prettyPrintFunMutDecl i (MutualDecls _ funDecls) = do
  putShow KwMutual
  printBlock i $
    sepBy "\n" (prettyPrintFunDecl $ i + tabWidth) funDecls

prettyPrintFunDecl :: Indentation -> FunDecl -> IO ()
prettyPrintFunDecl i (FunDecl _ funName argNames retTypeM varDecls stmts) = do
  printIndentation i
  T.putStr funName
  parens $ sepBy ", " T.putStr argNames
  case retTypeM of
    Nothing -> pure ()
    Just retType -> do
      spaces $ putShow SymColonColon
      prettyPrintType retType
  putChar ' '
  printBlock i (do
    sepBy "\n" (prettyPrintVarDecl $ i + tabWidth) varDecls
    unless (null varDecls || null stmts) $ putChar '\n'
    sepBy "\n" (prettyPrintStmt $ i + tabWidth) stmts)

-- | Since statements can be indented in code blocks, we always print `i`-many
-- spaces before the statement,
prettyPrintStmt :: Indentation -> Stmt -> IO ()
prettyPrintStmt i (If _ e stmts1 stmts2) = do
  printIndentation i
  putShow KwIf
  spaces $ parens $ prettyPrintExpr e
  printBlock i $ sepBy "\n" (prettyPrintStmt (i + tabWidth)) stmts1
  unless (null stmts2) $ do
    spaces $ putShow KwElse
    printBlock i $ sepBy "\n" (prettyPrintStmt (i + tabWidth)) stmts2
prettyPrintStmt i (While _ e stmts) = do
  printIndentation i
  putShow KwWhile
  spaces $ parens $ prettyPrintExpr e
  printBlock i $
    sepBy "\n" (prettyPrintStmt (i + tabWidth)) stmts
prettyPrintStmt i (Assign _ varLookup e) = do
  printIndentation i
  prettyPrintVarLookup varLookup
  spaces $ putShow SymEq
  prettyPrintExpr e
  putShow SymSemicolon
prettyPrintStmt i (FunCall _ funName args) = do
  printIndentation i
  T.putStr funName
  parens $ sepBy ", " prettyPrintExpr args
  putShow SymSemicolon
prettyPrintStmt i (Return _ eM) = do
  printIndentation i
  putStr $ show KwReturn
  case eM of
    Nothing -> pure ()
    Just e -> putChar ' ' >> prettyPrintExpr e
  putShow SymSemicolon
prettyPrintStmt i GarbageStmt = do
  printIndentation i
  putShow GarbageStmt >> putShow SymSemicolon

prettyPrintVarLookup :: VarLookup -> IO ()
prettyPrintVarLookup (VarId _ t) = T.putStr t
prettyPrintVarLookup (VarField _ varLookup field) =
  prettyPrintVarLookup varLookup >> prettyPrintField field

prettyPrintExprLookup :: ExprLookup -> IO ()
prettyPrintExprLookup (ExprField expr field) =
  prettyPrintExpr expr >> prettyPrintField field

prettyPrintField :: Field -> IO ()
prettyPrintField f = putShow SymDot >> putStr (field2Kw f)
  where
    field2Kw Head = show KwHead
    field2Kw Tail = show KwTail
    field2Kw Fst = show KwFst
    field2Kw Snd = show KwSnd
    field2Kw GarbageField = show GarbageField


prettyPrinter :: Program -> IO ()
prettyPrinter = prettyPrintProgram 0
