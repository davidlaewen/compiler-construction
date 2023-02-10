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
import Text.Megaparsec
import Parser.Tokens
import Data.Set qualified as S


-- TODO: Most of the combinators in Parser.Lexer should handle any trailing
-- whitespace so that minimal whitespace handling is necessary here.
-- Consistent whitespace parsing should also result in better error messages.

----------------------
-- Declarations


varDeclP :: TokenParser VarDecl
varDeclP = do
  var <- optional $ keywordP KwVar
  mty <- do
    case var of
      Nothing -> do Just <$> typeP
      Just () -> pure Nothing
  name <- identP
  _ <- symbolP SymEq
  expr <- exprP
  _ <- symbolP SymSemicolon
  return $ VarDecl mty name expr


funDeclP :: TokenParser FunDecl
funDeclP = do
  name <- identP
  params <- parensP (identP `sepBy` symbolP SymComma)
  retType <- optional (symbolP SymColonColon >> funTypeP)
  (decls, stmts) <- bracesP $ do
    decls <- many varDeclP
    stmts <- some stmtP
    pure (decls, stmts)
  pure $ FunDecl name params retType decls stmts


----------------------
-- Types

baseTypeP :: TokenParser Type
baseTypeP = intTypeP <|> boolTypeP <|> charTypeP
  where
    intTypeP = keywordP KwInt >> pure IntT
    boolTypeP = keywordP KwBool >> pure BoolT
    charTypeP = keywordP KwChar >> pure CharT


typeP :: TokenParser Type
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

funTypeP :: TokenParser Type
funTypeP = do
  argTys <- many typeP
  _ <- symbolP SymRightArrow
  Fun argTys <$> typeP



-------------------------
-- Expressions

exprP :: TokenParser Expr
exprP = empty


-------------------------
-- Statements

stmtP :: TokenParser Stmt
stmtP = empty


-------------------------
-- Programs

programP :: TokenParser Program
programP = empty


{-

nameReserved :: Text -> Lexer ()
nameReserved s | isKeyword s = fail . T.unpack $ "Keyword " <> s <> " cannot be used as an identifier."
               | otherwise = return ()


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

-}



----------------------------
-- Parser helpers
----------------------------

symbolP :: Symbol -> TokenParser ()
symbolP s = token test S.empty
  where
    test t | t == Symbol s = Just ()
    test _ = Nothing

keywordP :: Keyword -> TokenParser ()
keywordP k = token test S.empty
  where
    test t | t == Keyword k = Just ()
    test _ = Nothing

identP :: TokenParser Id
identP = token test S.empty
  where
    test (IdToken i) = Just i
    test _ = Nothing

intP :: TokenParser Expr
intP = token test S.empty
  where
    test (IntLit n) = Just $ Int n
    test _ = Nothing

boolP :: TokenParser Expr
boolP = token test S.empty
  where
    test (BoolLit b) = Just $ Bool b
    test _ = Nothing

charP :: TokenParser Expr
charP = token test S.empty
  where
    test (CharLit c) = Just $ Char c
    test _ = Nothing

-- | Parses expression of form (e), where e is parsed by the parser provided
--   in the argument.
parensP :: TokenParser a -> TokenParser a
parensP = between (symbolP SymParenLeft) (symbolP SymParenRight)

-- | Parses expression of form [e], where e is parsed by the parser provided
--   in the argument.
bracketsP :: TokenParser a -> TokenParser a
bracketsP = between (symbolP SymBracketLeft) (symbolP SymBracketRight)

-- | Parses expression of form {e}, where e is parsed by the parser provided
--   in the argument.
bracesP :: TokenParser a -> TokenParser a
bracesP = between (symbolP SymBraceLeft) (symbolP SymBraceRight)
