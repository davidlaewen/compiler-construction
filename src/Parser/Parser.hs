{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Parser.Parser (
  varDeclP,
  funDeclP,
  typeP,
  funTypeP,
  exprP,
  programP,
  parseExpr
) where

import Control.Applicative hiding (many,some)
import Control.Monad.Combinators.Expr
import Parser.Definition
import Syntax.Program
import Syntax.Types ( Type(..) )
import Text.Megaparsec
import Parser.Tokens ( Token(..), Keyword(..), Symbol(..) )
import Data.Set qualified as S
import Data.Text ( Text )
import Parser.Lexer (lexProgram)


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
  name <- idP
  _ <- symbolP SymEq
  expr <- exprP
  _ <- symbolP SymSemicolon
  return $ VarDecl mty name expr


funDeclP :: TokenParser FunDecl
funDeclP = do
  name <- idP
  params <- parensP (idP `sepBy` symbolP SymComma)
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
    tyVarP = TyVar <$> idP
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

fieldP :: Field -> TokenParser Field
fieldP f = do
  _ <- symbolP SymDot
  field <- headP <|> tailP <|> fstP <|> sndP
  res <- optional $ fieldP (field f)
  pure $ case res of
    Nothing -> field f
    Just f' -> f'
  where
    headP = keywordP KwHead >> pure Head
    tailP = keywordP KwTail >> pure Tail
    fstP = keywordP KwFst >> pure Fst
    sndP = keywordP KwSnd >> pure Snd

identP :: TokenParser Expr
identP = do
  ident <- Ident <$> idP
  field <- optional $ fieldP ident
  pure $ case field of
    Nothing -> Field ident
    Just f -> Field f

operatorTable :: [[Operator TokenParser Expr]]
operatorTable =
  [ [ prefix SymMinus (UnOp Neg),
      prefix SymBang  (UnOp Not) ],

    [ binary SymAst     (BinOp Mul),
      binary SymSlash   (BinOp Div),
      binary SymPercent (BinOp Mod) ],

    [ binary SymPlus  (BinOp Add),
      binary SymMinus (BinOp Sub) ],

    [ binary SymEqEq          (BinOp Eq),
      binary SymBangEq        (BinOp Neq),
      binary SymLessThan      (BinOp Lt),
      binary SymGreaterThan   (BinOp Gt),
      binary SymLessThanEq    (BinOp Lteq),
      binary SymGreaterThanEq (BinOp Gteq) ],

    [ binary SymPipePipe (BinOp Or),
      binary SymAndAnd   (BinOp And) ]

    -- TODO: Add cons operator to table
  ]
  where
    prefix sym f = Prefix (f <$ symbolP sym)
    binary sym f = InfixL (f <$ symbolP sym)

termP :: TokenParser Expr
termP = parensP exprP <|> intP <|> boolP <|> charP <|> emptyListP <|> identP
  where
    emptyListP = keywordP KwEmpty >> pure EmptyList

exprP :: TokenParser Expr
exprP = makeExprParser termP operatorTable


-- | Function for testing the combination of lexing and parsing for expressions
parseExpr :: Text -> Maybe Expr
parseExpr input =
  case runParser (lexProgram <* eof) "" input of
    Left _ -> Nothing
    Right ts -> case runParser (exprP <* eof) "" ts of
      Left _ -> Nothing
      Right e -> pure e


-------------------------
-- Statements

stmtP :: TokenParser Stmt
stmtP = empty


-------------------------
-- Programs

programP :: TokenParser Program
programP = empty


{-

Refactored expression grammar:

<Expr> := (!)* <Prop> (( && | || ) <Prop>)*

<Prop> := <Form> (( == | != | < | > | <= | >= ) <Form> )*
       |  <Bool>

<Form> := (-)* <Term> (( + | - ) <Term>)*
       |  <Char>

<Term> := <Val> (( * | / | % ) <Val>)*

<Val> := ( <Expr> ) | ( <Expr> , <Expr> )
      |  <FunCall> | <Id> <Field> | <Int> | []


Original grammar:

<Expr> := <Id> <Field>
       | <Expr> <Op2> <Expr>
       | <Op2> <Expr>
       | <Int>
       | <Char>
       | <Bool>
       | ( <Expr> )
       | <FunCall>
       | []
       | ( <Expr> , <Expr> )

<Op2> := + | - | * | / | % | == | < | > | <= | >= | != | && | || | :
<Op1> := ! | -




-}


{-

nameReserved :: Text -> Lexer ()
nameReserved s | isKeyword s = fail . T.unpack $ "Keyword " <> s <> " cannot be used as an identifier."
               | otherwise = return ()


--------------------------
-- Expressions

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

idP :: TokenParser Id
idP = token test S.empty
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
