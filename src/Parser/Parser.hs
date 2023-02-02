{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Parser.Parser (parseProgram) where

import Control.Applicative
import Parser.Definition
import Syntax.Common
import Syntax.Terms ( FunDecl(..), VarDecl(..), Stmt(..), Program(..) )
import Syntax.Types ( Type(..) )



parseFunDecl :: Parser FunDecl
parseFunDecl = do
  name <- parseId
  _ <- char '('
  params <- sepBy parseId (char ',')
  _ <- char ')'
  retType <- optional (do
    _ <- symbol "::"
    parseType)
  _ <- char '{'
  decls <- many parseVarDecl
  stmts <- many1 parseStmt
  _ <- char '}'
  return (FunDecl name params retType decls stmts)

parseType :: Parser Type
parseType = do
  return (TyVar "a")

parseVarDecl :: Parser VarDecl
parseVarDecl = failure

parseStmt :: Parser Stmt
parseStmt = failure

parseId :: Parser Id
parseId = identifier

parseProgram :: Parser Program
parseProgram = failure
