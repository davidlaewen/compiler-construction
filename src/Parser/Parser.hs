{-# LANGUAGE DeriveFunctor, InstanceSigs, FlexibleInstances, OverloadedStrings #-}
module Parser.Parser where

import Control.Applicative
import Control.Monad
import qualified Data.List
import Data.Char
import Parser.Definition
import Syntax.Common
import Syntax.Terms ( FunDecl(..) )
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
  return (FunDecl name params retType [] [])

parseType :: Parser Type
parseType = do
  return (TyVar "a")

parseVarDecl = pure Nothing

parseStmt = pure Nothing

parseId :: Parser Id
parseId = identifier
