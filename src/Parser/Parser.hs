{-# LANGUAGE FlexibleInstances #-}
module Parser.Parser (parser) where

import Control.Applicative hiding (many,some)
import Parser.Definition
import Syntax.Program
import Syntax.Types ( Type(..) )
import Text.Megaparsec
import Parser.Tokens ( Token(..), Keyword(..), Symbol(..) )
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Void

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
  Fun argTys <$> retTypeP
  where
    retTypeP :: TokenParser Type
    retTypeP = typeP <|> (keywordP KwVoid >> pure Void)


------------------------
-- Fields & Identifiers

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

identP :: TokenParser Field
identP = do
  ident <- Ident <$> idP
  field <- optional $ fieldP ident
  pure $ fromMaybe ident field


-------------------------
-- Expressions

funCallEP :: TokenParser Expr
funCallEP = do
  funId <- idP
  args <- parensP $ exprP `sepBy` symbolP SymComma
  pure $ FunCallE funId args

tupleP :: TokenParser Expr
tupleP = do
  parensP $ do
    e1 <- exprP
    _ <- symbolP SymComma
    Tuple e1 <$> exprP

valP :: TokenParser Expr
valP = try (parensP exprP) <|> tupleP <|>
          bangExprP <|> negExprP <|>
          intP <|> boolP <|> charP <|>
          emptyListP <|>
          try funCallEP <|>
          Field <$> identP
  where
    bangExprP = symbolP SymBang *> (UnOp Not <$> valP)
    negExprP = symbolP SymMinus *> (UnOp Neg <$> valP)
    emptyListP = symbolP SymBracketLR >> pure EmptyList

termP :: TokenParser Expr
termP = try (valP >>= opValP) <|> valP
  where
    opP :: TokenParser BinaryOp
    opP = (symbolP SymAst >> pure Mul) <|>
          (symbolP SymSlash >> pure Div) <|>
          (symbolP SymPercent >> pure Mod)
    opValP :: Expr -> TokenParser Expr
    opValP val = try (do
      op <- opP
      val' <- valP
      opValP (BinOp op val val')) <|> pure val

formP :: TokenParser Expr
formP = try (termP >>= opTermP) <|> termP
  where
    opP :: TokenParser BinaryOp
    opP = (symbolP SymPlus >> pure Add) <|>
          (symbolP SymMinus >> pure Sub)
    opTermP :: Expr -> TokenParser Expr
    opTermP term = try (do
      op <- opP
      term' <- termP
      opTermP (BinOp op term term')) <|> pure term

propP :: TokenParser Expr
propP = try (formP >>= opFormP) <|> formP
  where
    opP :: TokenParser BinaryOp
    opP = (symbolP SymEqEq >> pure Eq) <|>
          (symbolP SymBangEq >> pure Neq) <|>
          (symbolP SymLessThan >> pure Lt) <|>
          (symbolP SymGreaterThan >> pure Gt) <|>
          (symbolP SymLessThanEq >> pure Lteq) <|>
          (symbolP SymGreaterThanEq >> pure Gteq)
    opFormP :: Expr -> TokenParser Expr
    opFormP form = try (do
      op <- opP
      form' <- formP
      opFormP (BinOp op form form')) <|> pure form

{- Refactored expression grammar:

<Expr> :=  <Prop> (( && | || ) <Prop>)*

<Prop> := <Form> (( == | != | < | > | <= | >= ) <Form> )*

<Form> :=  <Term> (( + | - ) <Term>)*

<Term> := <Val> (( * | / | % ) <Val>)*

<Val> := ( <Expr> ) | ( <Expr> , <Expr> )
      |  <FunCall> | <Id> <Field> | <Int> | []
      | <Bool> | <Char> | !<Val> | - <Expr>
-}

exprP :: TokenParser Expr
exprP = try (propP >>= opPropP) <|> propP
  where
    opP :: TokenParser BinaryOp
    opP = (symbolP SymAndAnd >> pure And) <|>
          (symbolP SymPipePipe >> pure Or)
    opPropP :: Expr -> TokenParser Expr
    opPropP prop = try (do
      op <- opP
      prop' <- propP
      opPropP (BinOp op prop prop')) <|> pure prop

{- Original grammar:

<Expr> := <Id> <Field>
       | <Expr> <Op2> <Expr>
       | <Op2> <Expr>
       | <Int> | <Char> | <Bool>
       | ( <Expr> )
       | <FunCall>
       | []
       | ( <Expr> , <Expr> )

<Op2> := + | - | * | / | % | == | < | > | <= | >= | != | && | || | :
<Op1> := ! | -
-}

-------------------------
-- Statements

ifP :: TokenParser Stmt
ifP = do
  _ <- keywordP KwIf
  cond <- parensP exprP
  thenStmts <- bracesP (many stmtP)
  elseStmts <- optional $ do
    _ <- keywordP KwElse
    bracesP $ many stmtP
  pure $ If cond thenStmts (fromMaybe [] elseStmts)

whileP :: TokenParser Stmt
whileP = do
  _ <- keywordP KwWhile
  cond <- parensP exprP
  stmts <- bracesP $ many stmtP
  pure $ While cond stmts

assignP :: TokenParser Stmt
assignP = do
  name <- identP
  _ <- symbolP SymEq
  expr <- exprP
  _ <- symbolP SymSemicolon
  pure $ Assign name expr

funCallP :: TokenParser Stmt
funCallP = do
  funId <- idP <|> printP <|> isEmptyP
  args <- parensP $ exprP `sepBy` symbolP SymComma
  _ <- symbolP SymSemicolon
  pure $ FunCall funId args
  where
    -- TODO: Find a better to representation for pre-defined function names
    printP = keywordP KwPrint >> pure (T.pack $ show KwPrint)
    isEmptyP = keywordP KwIsEmpty >> pure (T.pack $ show KwIsEmpty)

returnP :: TokenParser Stmt
returnP = do
  _ <- keywordP KwReturn
  mExpr <- optional exprP
  _ <- symbolP SymSemicolon
  pure $ Return mExpr


stmtP :: TokenParser Stmt
stmtP = ifP <|> whileP <|> assignP <|> returnP <|> funCallP


-------------------------
-- Programs

programP :: TokenParser Program
programP = do
  varDecls <- many $ try varDeclP
  funDecls <- many funDeclP
  Program varDecls funDecls <$ eof


parser :: FilePath -> TokenStream -> Either (ParseErrorBundle TokenStream Void) Program
parser filePath input = snd $ runParser' programP $ initialState filePath input


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

-- FIXME: Refactor these to make them less cluncky
symbolP :: Symbol -> TokenParser ()
symbolP s = token test S.empty
  where
    test t | tokenVal t == Symbol s = Just ()
    test _ = Nothing

keywordP :: Keyword -> TokenParser ()
keywordP k = token test S.empty
  where
    test t | tokenVal t == Keyword k = Just ()
    test _ = Nothing

idP :: TokenParser Id
idP = token test S.empty
  where
    test (Positioned _ _ _ _ (IdToken i)) = Just i
    test _ = Nothing

intP :: TokenParser Expr
intP = token test S.empty
  where
    test (Positioned _ _ _ _ (IntLit n)) = Just $ Int n
    test _ = Nothing

boolP :: TokenParser Expr
boolP = token test S.empty
  where
    test (Positioned _ _ _ _ (BoolLit b)) = Just $ Bool b
    test _ = Nothing

charP :: TokenParser Expr
charP = token test S.empty
  where
    test (Positioned _ _ _ _ (CharLit c)) = Just $ Char c
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
