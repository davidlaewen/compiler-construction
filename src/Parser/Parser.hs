{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Parser.Parser (
  varDeclP,
  funDeclP,
  typeP,
  funTypeP,
  exprP
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
      _ <- symbolP SymEq
      sc
      expr <- exprP
      sc
      _ <- symbolP SymSemicolon
      return (VarDecl mty name expr)


funDeclP :: Parser FunDecl
funDeclP = do
  name <- identP
  sc
  params <- parensP (identP `sepBy` symbolP SymComma)
  sc
  retType <- optional (symbolP SymColonColon >> sc >> funTypeP)
  sc
  (decls, stmts) <- bracesP $ do
    decls <- many varDeclP
    stmts <- some stmtP
    pure (decls, stmts)
  pure (FunDecl name params retType decls stmts)


----------------------
-- Types

baseTypeP :: Parser Type
baseTypeP = intTypeP <|> boolTypeP <|> charTypeP
  where
    intTypeP = keywordP KwInt >> pure IntT
    boolTypeP = keywordP KwBool >> pure BoolT
    charTypeP = keywordP KwChar >> pure CharT

typeP :: Parser Type
typeP = baseTypeP <|> tyVarP <|> prodTypeP <|> listTypeP
  where
    tyVarP = TyVar <$> identP
    prodTypeP = do
      (ty1,ty2) <- parensP $ do
        ty1 <- typeP
        _ <- symbolP SymComma
        ty2 <- typeP
        pure (ty1,ty2)
      pure (Prod ty1 ty2)
    listTypeP = List <$> bracketsP typeP

funTypeP :: Parser Type
funTypeP = do
  argTys <- typeP `sepBy` scne -- Zero or more arg types
  _ <- symbolP SymRightArrow
  Fun argTys <$> typeP


-------------------------
-- Statements


stmtP :: Parser Stmt
stmtP = empty



--------------------------
-- Expressions


op1P :: Parser UnaryOp
op1P = notP <|> negP
  where
    notP = symbolP SymNot >> pure Not
    negP = symbolP SymNeg >> pure Neg

unOpP :: Parser Expr
unOpP = do
  op <- op1P
  sc
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
      e1 <- exprP -- TODO: Fix infinite recursion here
      sc
      op <- op2P
      sc
      BinOp op e1 <$> exprP


exprP :: Parser Expr
exprP = unOpP <|> parensP (exprP <* sc) <|> emptyList <|>
        int <|> char <|> bool <|> ident
  where
    ident = Field . Ident <$> identP
    int = Int <$> intP
    char = Char <$> charP
    bool = Bool <$> boolP
    emptyList = keywordP KwEmpty >> pure EmptyList



--------------------------
-- Programs

programP :: Parser Program
programP = empty
