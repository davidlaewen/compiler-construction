{-# LANGUAGE FlexibleInstances, OverloadedRecordDot, LambdaCase #-}

module Parser.Parser (parser) where

import Control.Applicative hiding (many)
import Parser.Definition
import Syntax.ParseAST
import Text.Megaparsec as MP
import Parser.Tokens (Token(..), Keyword(..), Symbol(..))
import qualified Data.Set as S
import qualified Data.Text as T
import Utils.Loc (Loc(..), HasLoc(..), defaultPos)
import Utils.Utils (fst3)
import Data.Maybe (fromMaybe)

----------------------
-- Declarations

varDeclP :: TokenParser Type -> TokenParser VarDecl
varDeclP typeP = do
  (mty,startPos) <- varOrTypeP
  (name,_,_) <- idP <|> registerError (withDummyPos (T.pack "")) VarDeclNoIdentifier
  _ <- symbolP SymEq <|> registerError (withDummyPos ()) VarDeclNoEquals
  -- TODO: In certain cases exprP inserts a garbage expr and doesn't fail, so parsing
  -- continues here even after an invalid expression, leading to confusing errors
  expr <- exprP <|> customFailure VarDeclNoExpression
  (_,_,endPos) <- symbolP SymSemicolon <|> registerError (withDummyPos ()) (NoClosingDelimiter SymSemicolon)
  pure $ VarDecl (Loc startPos endPos) mty name expr
  where
    varOrTypeP :: TokenParser (Maybe Type, SourcePos)
    varOrTypeP = (keywordP KwVar >>= \(_,start,_) -> pure (Nothing, start)) <|>
                 (typeP >>= \ty -> pure (Just ty, getStart ty))
      -- mVar <- optional $ keywordP KwVar
      -- case mVar of
      --   Just ((),start,_) -> pure (Nothing,start)
      --   Nothing -> monoTypeP >>= \ty -> pure (Just ty, getStart ty)

dataDeclP :: TokenParser DataDecl
dataDeclP = do
  (_,startPos,_) <- keywordP KwData
  (name,_,_) <- nameP
  (tyParams,_,_) <- anglesP ((fst3 <$> idP) `sepBy` symbolP SymComma) <|> pure (withDummyPos [])
  (constrs,_,endPos) <- bracesP $ dataConstrP `sepBy` symbolP SymComma
  pure $ DataDecl (Loc startPos endPos) name tyParams constrs

dataConstrP :: TokenParser DataConstr
dataConstrP = do
  (name,startPos,_) <- nameP
  (args,_,endPos) <- parensP $ constrArgP `sepBy` symbolP SymComma
  pure $ DataConstr (Loc startPos endPos) name args
  where
    constrArgP :: TokenParser (T.Text,Type)
    constrArgP = do
      (selector,_,_) <- idP
      _ <- symbolP SymColon
      ty <- polyTypeP -- We currently only support monomorphic constructors
      pure (selector,ty)

funDeclP :: TokenParser FunDecl
funDeclP = do
  funIdOffset <- getOffset
  (name,startPos,_) <- idP
  (params,_,_) <- parensP ((fst3 <$> idP) `sepBy` symbolP SymComma)
  retType <- optional (symbolP SymColonColon >> funTypeP)
  ((decls,stmts),_,endPos) <- bracesP $ do
    decls <- many $ varDeclP monoTypeP -- (try isVarDeclLookahead >> varDeclP)
    stmts <- many (withRecovery parseToNextStmt stmtP)
    pure (decls, stmts)
  handleNoStatements funIdOffset stmts
  pure $ FunDecl (Loc startPos endPos) name params retType decls stmts
  where
    handleNoStatements :: Int -> [Stmt] -> TokenParser ()
    handleNoStatements offset [] = registerErrorOffset () FunctionMissingStatements offset
    handleNoStatements _ _ = pure ()


----------------------
-- Types

baseTypeP :: TokenParser Type
baseTypeP = intTypeP <|> boolTypeP <|> charTypeP
  where
    intTypeP = keywordP KwInt >>= (\(_,start,end) -> pure $ IntT (Loc start end))
    boolTypeP = keywordP KwBool >>= (\(_,start,end) -> pure $ BoolT (Loc start end))
    charTypeP = keywordP KwChar >>= (\(_,start,end) -> pure $ CharT (Loc start end))

prodTypeP :: TokenParser Type -> TokenParser Type
prodTypeP innerP = do
  (_,start,_) <- symbolP SymParenLeft
  ty1 <- innerP <|> customFailure ProdTypeMissingEntry
  _ <- symbolP SymComma <|> registerError (withDummyPos ()) ProdTypeMissingComma
  ty2 <- innerP <|> customFailure ProdTypeNoSecondEntry
  (_,_,end) <- symbolP SymParenRight <|> registerError (withDummyPos ()) (NoClosingDelimiter SymParenRight)
  pure $ Prod (Loc start end) ty1 ty2

listTypeP :: TokenParser Type -> TokenParser Type
listTypeP innerP = do
  (_,start,_) <- symbolP SymBracketLeft
  ty <- innerP <|> customFailure ListTypeMissingEntry
  (_,_,end) <- symbolP SymBracketRight <|> registerError (withDummyPos ()) (NoClosingDelimiter SymBracketRight)
  pure $ List (Loc start end) ty

dataTypeP :: TokenParser Type -> TokenParser Type
dataTypeP innerP = do
  (name,start,end) <- nameP
  (anglesP (innerP `sepBy` symbolP SymComma) >>= \(tyArgs,_,end') ->
      pure $ DataT (Loc start end') name tyArgs)
    <|> pure (DataT (Loc start end) name [])
  -- optional (anglesP $ innerP `sepBy` symbolP SymComma) >>= \case
  --   Nothing -> pure $ DataT (Loc start end) name []
  --   Just (tyArgs,_,end') -> pure $ DataT (Loc start end') name tyArgs

tyVarP :: TokenParser Type
tyVarP = idP >>= \(ident,start,end) -> pure $ TyVar (Loc start end) ident

monoTypeP :: TokenParser Type
monoTypeP = baseTypeP <|> dataTypeP monoTypeP <|> prodTypeP monoTypeP <|> listTypeP monoTypeP

polyTypeP :: TokenParser Type
polyTypeP = baseTypeP <|> tyVarP <|> dataTypeP polyTypeP <|>
            prodTypeP polyTypeP <|> listTypeP polyTypeP

-- | Parses a polymorphic type, but disallows type paramaters at the outermost
-- level, i.e. they can only appear inside a product, list or data type
varTypeP :: TokenParser Type
varTypeP = baseTypeP <|> dataTypeP polyTypeP <|> prodTypeP polyTypeP <|> listTypeP polyTypeP

funTypeP :: TokenParser Type
funTypeP = do
  argTys <- many polyTypeP
  (_,start,_) <- symbolP SymRightArrow <|> registerError (withDummyPos ()) FunctionNoArrow
  if start == defaultPos then pure GarbageType else do
    retTy <- retTypeP <|> registerError GarbageType FunctionNoRetType
    let startPos = case argTys of
          [] -> start
          (ty:_) -> getStart ty
    pure $ Fun (Loc startPos (getEnd retTy)) argTys retTy
  where
    retTypeP :: TokenParser Type
    retTypeP = polyTypeP <|> (keywordP KwVoid >>= \(_,start,end) -> pure $ Void (Loc start end))


------------------------
-- Fields & Identifiers

fieldP :: TokenParser (WithPos Field)
fieldP = headP <|> tailP <|> fstP <|> sndP <|> selectorP
  where
    headP = keywordP KwHead >>= \(_,s,e) -> pure (Head,s,e)
    tailP = keywordP KwTail >>= \(_,s,e) -> pure (Tail,s,e)
    fstP = keywordP KwFst >>= \(_,s,e) -> pure (Fst,s,e)
    sndP = keywordP KwSnd >>= \(_,s,e) -> pure (Snd,s,e)
    selectorP = idP >>= \(t,s,e) -> pure (Selector t,s,e)

-------------------------
-- Expressions

funCallEP :: TokenParser Expr
funCallEP = do
  ((funId,args),start,end) <- funCallP
  pure $ FunCallE (Loc start end) funId args

constrCallP :: TokenParser Expr
constrCallP = do
  (name,start,_) <- nameP
  (args,_,end) <- parensP $ exprP `sepBy` symbolP SymComma
  pure $ ConstrCall (Loc start end) name args


parenOrTupleP :: TokenParser Expr
parenOrTupleP = do
  (_,start,_) <- symbolP SymParenLeft
  e <- exprP <|> registerError GarbageExpr ParenOpenNoExpression
  closeExpr e <|> closeTuple e start
  where
    closeExpr :: Expr -> TokenParser Expr
    closeExpr e = symbolP SymParenRight >> pure e
    closeTuple :: Expr -> SourcePos -> TokenParser Expr
    closeTuple e1 start = do
      _ <- symbolP SymComma <|> registerError (withDummyPos ()) TupleNoComma
      e2 <- exprP
      (_,_,end) <- symbolP SymParenRight <|>
        registerError ((),defaultPos,getEnd e2) ParenOpenNotClosed
      pure $ Tuple (Loc start end) e1 e2

atomP :: TokenParser Expr
atomP = parenOrTupleP <|>
        intP <|> boolP <|> charP <|>
        emptyListP <|>
        constrCallP <|>
        try funCallEP <|> identP
  where
    emptyListP = do
      (_,start,_) <- symbolP SymBracketLeft
      (_,_,end) <- symbolP SymBracketRight <|> registerError (withDummyPos ()) (NoClosingDelimiter SymBracketRight)
      pure $ EmptyList (Loc start end)
    identP = idP >>= \(ident,s,e) -> pure $ Ident (Loc s e) ident

fieldLookupP :: TokenParser Expr
fieldLookupP = atomP >>= opFieldLookupP
  where
    opFieldLookupP :: Expr -> TokenParser Expr
    opFieldLookupP e = try (do
      (_,start,_) <- symbolP SymDot
      (field,_,end) <- fieldP <|> registerError (withDummyPos GarbageField) FieldLookupNoField
      opFieldLookupP (ExprLookup (Loc start end) (ExprField e field))) <|> pure e

unOpP :: TokenParser Expr
unOpP = bangExprP <|> negExprP <|> fieldLookupP
  where
    bangExprP = symbolP SymBang >>= nextUnOpP Not
    negExprP = symbolP SymMinus >>= nextUnOpP Neg
    nextUnOpP op (_,s,e) =
      UnOp (Loc s e) op <$> unOpP <|> registerError GarbageExpr (UnaryOpNoExpression op)

termP :: TokenParser Expr
termP = unOpP >>= opValP
  where
    opP :: TokenParser BinaryOp
    opP = (symbolP SymAst >> pure Mul) <|>
          (symbolP SymSlash >> pure Div) <|>
          (symbolP SymPercent >> pure Mod)
    opValP :: Expr -> TokenParser Expr
    opValP val = (try opP >>= \op -> do
      val' <- unOpP <|> registerError GarbageExpr (BinaryOpNoExpression op)
      opValP (BinOp (Loc (getStart val) (getEnd val')) op val val')) <|> pure val

formP :: TokenParser Expr
formP = termP >>= opTermP
  where
    opP :: TokenParser BinaryOp
    opP = (symbolP SymPlus >> pure Add) <|>
          (symbolP SymMinus >> pure Sub)
    opTermP :: Expr -> TokenParser Expr
    opTermP term = (try opP >>= \op -> do
      term' <- termP <|> registerError GarbageExpr (BinaryOpNoExpression op)
      opTermP (BinOp (Loc (getStart term) (getEnd term')) op term term')) <|> pure term

listP :: TokenParser Expr
listP = formP >>= opFormP
  where
    opP :: TokenParser BinaryOp
    opP = symbolP SymColon >> pure Cons
    -- Cons operator `:` associates to the right
    opFormP :: Expr -> TokenParser Expr
    opFormP form = (try opP >>= \op -> do
      list <- listP -- Recurse first to determine end pos
        <|> registerError GarbageExpr ConsNoExpression
      pure $ BinOp (Loc (getStart form) (getEnd list)) op form list) <|> pure form

propP :: TokenParser Expr
propP = listP >>= opListP
  where
    opP :: TokenParser BinaryOp
    opP = (symbolP SymEqEq >> pure Eq) <|>
          (symbolP SymBangEq >> pure Neq) <|>
          (symbolP SymLessThan >> pure Lt) <|>
          (symbolP SymGreaterThan >> pure Gt) <|>
          (symbolP SymLessThanEq >> pure Lte) <|>
          (symbolP SymGreaterThanEq >> pure Gte)
    opListP :: Expr -> TokenParser Expr
    opListP list = (try opP >>= \op -> do
      list' <- listP <|> registerError GarbageExpr (BinaryOpNoExpression op)
      opListP (BinOp (Loc (getStart list) (getEnd list')) op list list')) <|> pure list

{- Refactored expression grammar:

<Expr> :=  <Prop> (( && | || ) <Prop>)*

<Prop> := <List> (( == | != | < | > | <= | >= ) <List> )*

<List> := <Form> : <List>

<Form> := <Term> (( + | - ) <Term>)*

<Term> := <Val> (( * | / | % ) <Val>)*

<UnOp> := ! <UnOp> | - <UnOp> | <Sel>

<Sel> := <Atom> (. <Field>)*

<Atom> := ( <Expr> ) | ( <Expr> , <Expr> ) | <Bool> | <Char> | <Int> | []
       | <Id> | <FunCall>
-}

exprP :: TokenParser Expr
exprP = propP >>= opPropP
  where
    opP :: TokenParser BinaryOp
    opP = (symbolP SymAndAnd >> pure And) <|>
          (symbolP SymPipePipe >> pure Or)
    opPropP :: Expr -> TokenParser Expr
    opPropP prop = (try opP >>= \op -> do
      prop' <- propP <|> registerError GarbageExpr (BinaryOpNoExpression op)
      opPropP (BinOp (Loc (getStart prop) (getEnd prop')) op prop prop')) <|> pure prop

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

-- | Consumes tokens up until either `;` or `}`, in an attempt to find the start
-- of the next statement, and parse it (again with recovery, by recursion).
-- Recovery should fail if the next token is `}`, that is, when we have reached
-- the end of the current statement block, otherwise the recovery will 'escape'
-- from the current statement block and begin eating the rest of the program,
-- leading to other errors that no longer correspond to the source program.
-- NOTE: In order to recover parsing after an invalid statement, we need to
-- encounter a valid statement before the end of the current block. If all
-- remaining statements are invalid, or there are none, the recovery will fail
-- and we only report the original error.
parseToNextStmt :: ParseError TokenStream ParserError -> TokenParser Stmt
parseToNextStmt e = (symbolP SymBraceRight >> fail "") <|>
  (skipManyTill (satisfy $ const True)
      (symbolP SymSemicolon <|> symbolP SymBraceRight) >>
    registerParseError e >> withRecovery parseToNextStmt stmtP)

ifP :: TokenParser Stmt
ifP = do
  (_,startPos,_) <- keywordP KwIf
  (cond,_,_) <- parensP exprP <|> registerError (withDummyPos GarbageExpr) IfNoCondition
  -- _ <- symbolP SymBraceLeft <|> registerError (withDummyPos ()) IfNoOpenBrace
  (thenStmts,_,end) <- bracesP $ many (withRecovery parseToNextStmt stmtP)
  -- (_,_,end) <- symbolP SymBraceRight <|> registerError (withDummyPos ()) (NoClosingDelimiter SymBraceRight)
  (elseStmts,endPos) <- fromMaybe ([],end) <$> optional (do
    _ <- keywordP KwElse
    (stmts,_,end') <- bracesP $ many (withRecovery parseToNextStmt stmtP)
    pure (stmts,end'))
  -- let (elseStmts,endPos) = case mElseStmts of
  --       Nothing -> ([],end)
  --       Just (stmts,_,end') -> (stmts,end')
  pure $ If (Loc startPos endPos) cond thenStmts elseStmts

whileP :: TokenParser Stmt
whileP = do
  (_,start,_) <- keywordP KwWhile
  (cond,_,_) <- parensP exprP
  (stmts,_,end) <- bracesP $ many (withRecovery parseToNextStmt stmtP)
  pure $ While (Loc start end) cond stmts

assignP :: TokenParser Stmt
assignP = do
  (name,start,end) <- idP
  var <- varFieldsP (VarId (Loc start end) name)
  _ <- symbolP SymEq
  expr <- exprP
  (_,_,endPos) <- symbolP SymSemicolon
  pure $ Assign (Loc start endPos) var expr
  where
    varFieldsP :: VarLookup -> TokenParser VarLookup
    varFieldsP vl = try (do
      _ <- symbolP SymDot
      (field,_,end) <- fieldP
      varFieldsP (VarField (Loc (getStart vl) end) vl field)) <|> pure vl


funCallSP :: TokenParser Stmt
funCallSP = do
  ((funId,args),start,_) <- funCallP
  (_,_,end) <- symbolP SymSemicolon
  pure $ FunCall (Loc start end) funId args

returnP :: TokenParser Stmt
returnP = do
  (_,start,_) <- keywordP KwReturn
  mExpr <- optional exprP
  (_,_,end) <- symbolP SymSemicolon
  pure $ Return (Loc start end) mExpr


stmtP :: TokenParser Stmt
stmtP = ifP <|> whileP <|> returnP <|> try assignP <|> funCallSP


-------------------------
-- Programs

programP :: TokenParser Program
programP = do
  -- All varDecls must appear before all funDecls
  dataDecls <- many dataDeclP
  varDecls <- many $ varDeclP monoTypeP
  funDecls <- many funDeclP
  handleNoFunDecls funDecls
  Program dataDecls varDecls funDecls <$ eof
  where
    handleNoFunDecls :: [FunDecl] -> TokenParser ()
    handleNoFunDecls [] = registerError () ProgramNoFunDecls
    handleNoFunDecls _ = pure ()


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

registerErrorOffset :: a -> ParserError -> Int -> TokenParser a
registerErrorOffset x e o = region (setErrorOffset o) $
  registerFancyFailure (S.singleton $ ErrorCustom e) >> pure x

registerError :: a -> ParserError -> TokenParser a
registerError x e = getOffset >>= registerErrorOffset x e

withDummyPos :: a -> WithPos a
withDummyPos x = (x,defaultPos,defaultPos)

symbolP :: Symbol -> TokenParser (WithPos ())
symbolP s = token test S.empty
  where
    test t | tokenVal t == Symbol s = Just ((), t.startPosition, t.endPosition)
    test _ = Nothing

keywordP :: Keyword -> TokenParser (WithPos ())
keywordP k = token test S.empty
  where
    test t | tokenVal t == Keyword k = Just ((), t.startPosition, t.endPosition)
    test _ = Nothing

idP :: TokenParser (WithPos T.Text)
idP = token test S.empty
  where
    test (Positioned start end _ _ (IdToken i)) = Just (i,start,end)
    test _ = Nothing

nameP :: TokenParser (WithPos T.Text)
nameP = token test S.empty
  where
    test (Positioned start end _ _ (NameToken name)) = Just (name,start,end)
    test _ = Nothing

intP :: TokenParser Expr
intP = token test S.empty
  where
    test (Positioned start end _ _ (IntLit n)) = Just $ Int (Loc start end) n
    test _ = Nothing

boolP :: TokenParser Expr
boolP = token test S.empty
  where
    test (Positioned start end _ _ (BoolLit b)) = Just $ Bool (Loc start end) b
    test _ = Nothing

charP :: TokenParser Expr
charP = token test S.empty
  where
    test (Positioned start end _ _ (CharLit c)) = Just $ Char (Loc start end) c
    test _ = Nothing

funCallP :: TokenParser (WithPos (T.Text, [Expr]))
funCallP = do
  (funId,start,_) <- idP
  (args,_,end) <- parensP $ exprP `sepBy` symbolP SymComma
  pure ((funId, args), start, end)

betweenP :: TokenParser (WithPos open) -> TokenParser (WithPos close) -> TokenParser a -> TokenParser (WithPos a)
betweenP openP closeP p = do
  (_,start,_) <- openP
  x <- p
  (_,_,end) <- closeP
  pure (x,start,end)

-- | Parses expression of form `(e)`, where e is parsed by the parser provided
--   in the argument.
parensP :: TokenParser a -> TokenParser (WithPos a)
parensP = betweenP (symbolP SymParenLeft) (symbolP SymParenRight)

-- | Parses expression of form `{e}`, where e is parsed by the parser provided
--   in the argument.
bracesP :: TokenParser a -> TokenParser (WithPos a)
bracesP = betweenP (symbolP SymBraceLeft) (symbolP SymBraceRight)

-- | Parses object of form `< a >`, where a is parsed by the parser provided
--   in the argument.
anglesP :: TokenParser a -> TokenParser (WithPos a)
anglesP = betweenP (symbolP SymLessThan) (symbolP SymGreaterThan)
