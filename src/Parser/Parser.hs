{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Parser.Parser (
  varDeclP,
  funDeclP
) where

import Control.Applicative hiding (many,some)
import Parser.Definition
import Syntax.Program
import Syntax.Types ( Type(..) )
import Parser.Lexer
import Text.Megaparsec


-- TODO: Most of the combinators in Parser.Lexer should handle any trailing
-- whitespace so that minimal whitespace handling is necessary here.
-- Consistent whitespace parsing should also result in better error messages.

varDeclP :: Parser VarDecl
varDeclP = do
      var <- optional $ keywordP KwVar
      mty <- do
        case var of
          Nothing -> do Just <$> typeP
          Just () -> pure Nothing
      sc
      name <- identP
      sc
      _ <- symbolP SymColon
      sc
      expr <- exprP
      sc
      _ <- symbolP SymSemicolon
      return (VarDecl mty name expr)


funDeclP :: Parser FunDecl
funDeclP = do
  name <- identP
  sc
  params <- parensP (identP `sepBy` scne)
  sc
  retType <- optional (symbolP SymComma >> sc >> typeP)
  sc
  (decls, stmts) <- bracesP $ do
    decls <- many varDeclP
    stmts <- some stmtP
    pure (decls, stmts)
  pure (FunDecl name params retType decls stmts)


typeP :: Parser Type
typeP = do
  return (TyVar "a")

stmtP :: Parser Stmt
stmtP = empty

op1P :: Parser UnaryOp
op1P = notP <|> negP
  where
    notP = symbolP SymNot >> pure Not
    negP = symbolP SymNeg >> pure Neg

unOpP :: Parser Expr
unOpP = do
  op <- op1P
  UnOp op <$> exprP


op2P :: Parser BinaryOp
op2P = plusP <|> minusP
  where
    plusP = symbolP SymPlus >> pure Add
    minusP = symbolP SymMinus >> pure Sub


-- TODO: We likely need to handle the operators individually to get the
-- desired associativity properties, e.g. parsing x*y+z as (x*y)+z or
-- parsing x/y/z as (x/y)/z
binOpP :: Parser Expr
binOpP = do
      e1 <- exprP
      op <- op2P
      BinOp op e1 <$> exprP


exprP :: Parser Expr
exprP = int <|> char <|> bool <|> unOpP <|> binOpP
  where
    int = Int <$> intP
    char = Char <$> charP
    bool = Bool <$> boolP




programP :: Parser Program
programP = empty
