{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Parser.Parser (parser) where

import Control.Applicative hiding (many,some)
import Parser.Definition
import Syntax.ParseAST
import Text.Megaparsec
import Parser.Tokens ( Token(..), Keyword(..), Symbol(..) )
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.Either (partitionEithers)
import Control.Monad
import qualified Data.Text as T

----------------------
-- Declarations

varDeclP :: TokenParser VarDecl
varDeclP = do
  var <- optional $ keywordP KwVar
  mty <-
    case var of
      Nothing -> Just <$> typeP
      Just () -> pure Nothing
  name <- idP
  _ <- symbolP SymEq
  expr <- exprP
  _ <- symbolP SymSemicolon
  pure $ VarDecl mty name expr


funDeclP :: TokenParser FunDecl
funDeclP = do
  funIdOffset <- getOffset
  name <- idP
  params <- parensP (idP `sepBy` symbolP SymComma)
  retType <- optional (symbolP SymColonColon >> funTypeP)
  (decls, stmts) <- bracesP $ do
    decls <- many (try varDeclP)
    stmts <- many stmtP >>= handleNoStatements funIdOffset
    pure (decls, stmts)
  pure $ FunDecl name params retType decls stmts
  where
    handleNoStatements :: Int -> [Stmt] -> TokenParser [Stmt]
    handleNoStatements o [] = region (setErrorOffset o) $
      registerFancyFailure (S.singleton $
        ErrorCustom FunctionMissingStatements) >> pure [GarbageS]
    handleNoStatements _ stmts = pure stmts



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
        _ <- symbolP SymComma <|> customFailure ProdTypeMissingComma
        ty2 <- typeP <|> customFailure ProdTypeNoSecondEntry
        pure (ty1,ty2)
      pure (Prod ty1 ty2)
    listTypeP = List <$> bracketsP typeP

funTypeP :: TokenParser Type
funTypeP = do
  argTys <- many typeP
  _ <- symbolP SymRightArrow
  offset <- getOffset
  Fun argTys <$> (retTypeP <|> registerError offset GarbageT NoRetType)
  where
    retTypeP :: TokenParser Type
    retTypeP = typeP <|> (keywordP KwVoid >> pure Void)


------------------------
-- Fields & Identifiers

fieldP :: TokenParser Field
fieldP = headP <|> tailP <|> fstP <|> sndP
  where
    headP = keywordP KwHead >> pure Head
    tailP = keywordP KwTail >> pure Tail
    fstP = keywordP KwFst >> pure Fst
    sndP = keywordP KwSnd >> pure Snd

-------------------------
-- Expressions

funCallEP :: TokenParser Expr
funCallEP = do
  (funId,args) <- funCallP
  pure $ FunCallE funId args

parenOrTupleP :: TokenParser Expr
parenOrTupleP = do
  _ <- symbolP SymParenLeft
  e <- exprP
  closeExpr e <|> closeTuple e
  where
    closeExpr :: Expr -> TokenParser Expr
    closeExpr e = do
      _  <- symbolP SymParenRight
      pure e
    closeTuple :: Expr -> TokenParser Expr
    closeTuple e1 = do
      _ <- symbolP SymComma
      e2 <- exprP
      _ <- symbolP SymParenRight
      pure $ Tuple e1 e2

-- atomP :: TokenParser Expr
-- <Val> (. <Field)*

{- Refactored expression grammar:

<Expr> :=  <Prop> (( && | || ) <Prop>)*

<Prop> := <List> (( == | != | < | > | <= | >= ) <List> )*

<List> := <Form> ( :  <Form> )*

<Form> := <Term> (( + | - ) <Term>)*

<Term> := <Val> (( * | / | % ) <Val>)*

<UnOp> := !<Sel> | - <Sel> | <Sel>

<Sel> := <Atom> (. <Field>)*

<Atom> := ( <Expr> ) | ( <Expr> , <Expr> ) | <Bool> | <Char> | <Int> | []
       | <Id> | <FunCall>
-}

atomP :: TokenParser Expr
atomP = parenOrTupleP <|>
        intP <|> boolP <|> charP <|>
        emptyListP <|>
        try funCallEP <|> (Ident <$> idP)
  where
    emptyListP = symbolP SymBracketLR >> pure EmptyList

fieldLookupP :: TokenParser Expr
fieldLookupP = atomP >>= opFieldLookupP
  where
    opFieldLookupP :: Expr -> TokenParser Expr
    opFieldLookupP e = try (do
      _ <- symbolP SymDot
      field <- fieldP
      opFieldLookupP (ExprLookup (ExprField e field))) <|> pure e

unOpP :: TokenParser Expr
unOpP = bangExprP <|> negExprP <|> fieldLookupP
  where
    bangExprP = symbolP SymBang *> (UnOp Not <$> unOpP)
    negExprP = symbolP SymMinus *> (UnOp Neg <$> unOpP)

termP :: TokenParser Expr
termP = try (unOpP >>= opValP) <|> unOpP
  where
    opP :: TokenParser BinaryOp
    opP = (symbolP SymAst >> pure Mul) <|>
          (symbolP SymSlash >> pure Div) <|>
          (symbolP SymPercent >> pure Mod)
    opValP :: Expr -> TokenParser Expr
    opValP val = try (do
      op <- opP
      val' <- unOpP
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

listP :: TokenParser Expr
listP = try (formP >>= opFormP) <|> formP
  where
    opP :: TokenParser BinaryOp
    opP = symbolP SymColon >> pure Cons
    opFormP :: Expr -> TokenParser Expr
    opFormP form = try (do
      op <- opP
      form' <- formP
      opFormP (BinOp op form form')) <|> pure form

propP :: TokenParser Expr
propP = try (listP >>= opListP) <|> listP
  where
    opP :: TokenParser BinaryOp
    opP = (symbolP SymEqEq >> pure Eq) <|>
          (symbolP SymBangEq >> pure Neq) <|>
          (symbolP SymLessThan >> pure Lt) <|>
          (symbolP SymGreaterThan >> pure Gt) <|>
          (symbolP SymLessThanEq >> pure Lte) <|>
          (symbolP SymGreaterThanEq >> pure Gte)
    opListP :: Expr -> TokenParser Expr
    opListP list = try (do
      op <- opP
      list' <- listP
      opListP (BinOp op list list')) <|> pure list

{- Refactored expression grammar:

<Expr> :=  <Prop> (( && | || ) <Prop>)*

<Prop> := <List> (( == | != | < | > | <= | >= ) <List> )*

<List> := <Form> ( :  <Form> )*

<Form> := <Term> (( + | - ) <Term>)*

<Term> := <Val> (( * | / | % ) <Val>)*

<UnOp> := ! <UnOp> | - <UnOp> | <Sel>

<Sel> := <Atom> (. <Field>)*

<Atom> := ( <Expr> ) | ( <Expr> , <Expr> ) | <Bool> | <Char> | <Int> | []
       | <Id> | <FunCall>
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
  name <- idP
  var <- varFieldsP (VarId name)
  _ <- symbolP SymEq
  expr <- exprP
  _ <- symbolP SymSemicolon
  pure $ Assign var expr
  where
    varFieldsP :: VarLookup -> TokenParser VarLookup
    varFieldsP e = try (do
      _ <- symbolP SymDot
      field <- fieldP
      varFieldsP (VarField e field)) <|> pure e


funCallSP :: TokenParser Stmt
funCallSP = do
  (funId,args) <- funCallP
  _ <- symbolP SymSemicolon
  pure $ FunCall funId args

returnP :: TokenParser Stmt
returnP = do
  _ <- keywordP KwReturn
  mExpr <- optional exprP
  _ <- symbolP SymSemicolon
  pure $ Return mExpr


stmtP :: TokenParser Stmt
stmtP = ifP <|> whileP <|> returnP <|> try assignP <|> funCallSP


-------------------------
-- Programs

programP :: TokenParser Program
programP = do
  (funDecls, varDecls) <- partitionEithers <$> many ((Left <$> (lookAhead isFunDeclLookAhead >> funDeclP)) <|> (Right <$> varDeclP))
  Program varDecls funDecls <$ eof
  where
    isFunDeclLookAhead :: TokenParser ()
    isFunDeclLookAhead = void $ satisfy isIdent >> satisfy isOpenParen

    isIdent :: Positioned Parser.Tokens.Token -> Bool
    isIdent (Positioned _ _ _ _ (IdToken _)) = True
    isIdent _ = False

    isOpenParen :: Positioned Parser.Tokens.Token -> Bool
    isOpenParen (Positioned _ _ _ _ (Symbol SymParenLeft)) = True
    isOpenParen _ = False


parser :: FilePath -> TokenStream -> Either (ParseErrorBundle TokenStream ParserError) Program
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

registerError :: Int -> a -> ParserError -> TokenParser a
registerError o x e = region (setErrorOffset o) $
  registerFancyFailure (S.singleton $ ErrorCustom e)
    >> pure x

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

idP :: TokenParser T.Text
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

funCallP :: TokenParser (T.Text, [Expr])
funCallP = do
  funId <- idP
  args <- parensP $ exprP `sepBy` symbolP SymComma
  pure (funId, args)

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
