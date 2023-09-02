{-# LANGUAGE FlexibleInstances #-}

module Parser.Parser (parser) where

import Control.Applicative hiding (many,some)
import Parser.Definition
import Syntax.ParseAST
import Text.Megaparsec
import Parser.Tokens ( Token(..), Keyword(..), Symbol(..) )
import qualified Data.Set as S
import Data.Either (partitionEithers)
import Control.Monad
import qualified Data.Text as T
import Utils.Loc (Loc(..), HasLoc(..))
import Utils.Utils (fst3)

----------------------
-- Declarations

varDeclP :: TokenParser VarDecl
varDeclP = do
  mVar <- optional $ keywordP KwVar
  (mty,startPos) <- case mVar of
    Nothing -> do
      ty <- typeP
      pure (Just ty, getStart ty)
    Just ((),start,_) -> pure (Nothing,start)
  (name,_,_) <- idP
  _ <- symbolP SymEq
  expr <- exprP
  (_,_,endPos) <- symbolP SymSemicolon
  pure $ VarDecl (Loc startPos endPos) mty name expr


funDeclP :: TokenParser FunDecl
funDeclP = do
  funIdOffset <- getOffset
  (name,startPos,_) <- idP
  (params,_,_) <- parensP ((idP `sepBy` symbolP SymComma) >>=
    \t -> pure (fst3 <$> t))
  retType <- optional (symbolP SymColonColon >> funTypeP)
  ((decls, stmts),_,endPos) <- bracesP $ do
    decls <- many (try varDeclP)
    stmts <- many stmtP >>= handleNoStatements funIdOffset
    pure (decls, stmts)
  pure $ FunDecl (Loc startPos endPos) name params retType decls stmts
  where
    handleNoStatements :: Int -> [Stmt] -> TokenParser [Stmt]
    handleNoStatements o [] = region (setErrorOffset o) $
      registerFancyFailure (S.singleton $
        ErrorCustom FunctionMissingStatements) >> pure [GarbageS]
    handleNoStatements _ stmts = pure stmts


mutualDeclP :: TokenParser FunMutDecl
mutualDeclP = do
  (_,start,_) <- keywordP KwMutual
  (funDecls,_,end) <- bracesP $ many funDeclP
  pure $ MutualDecls (Loc start end) funDecls

funOrMutualDeclP :: TokenParser FunMutDecl
funOrMutualDeclP = mutualDeclP <|> SingleDecl <$> funDeclP


----------------------
-- Types

baseTypeP :: TokenParser Type
baseTypeP = intTypeP <|> boolTypeP <|> charTypeP
  where
    intTypeP = keywordP KwInt >>= (\(_,start,end) -> pure $ IntT (Loc start end))
    boolTypeP = keywordP KwBool >>= (\(_,start,end) -> pure $ BoolT (Loc start end))
    charTypeP = keywordP KwChar >>= (\(_,start,end) -> pure $ CharT (Loc start end))

typeP :: TokenParser Type
typeP = baseTypeP <|> tyVarP <|> prodTypeP <|> listTypeP
  where
    tyVarP = idP >>= \(ident,start,end) ->
      pure $ TyVar (Loc start end) ident
    prodTypeP = do
      ((ty1,ty2),start,end) <- parensP $ do
        ty1 <- typeP
        _ <- symbolP SymComma <|> customFailure ProdTypeMissingComma
        ty2 <- typeP <|> customFailure ProdTypeNoSecondEntry
        pure (ty1,ty2)
      pure $ Prod (Loc start end) ty1 ty2
    listTypeP = bracketsP typeP >>= \(ty,start,end) ->
      pure $ List (Loc start end) ty

funTypeP :: TokenParser Type
funTypeP = do
  argTys <- many typeP
  (_,start,_) <- symbolP SymRightArrow
  offset <- getOffset
  retTy <- retTypeP <|> registerError offset GarbageT NoRetType
  let startPos = case argTys of
        [] -> start
        (ty:_) -> getStart ty
  pure $ Fun (Loc startPos (getEnd retTy)) argTys retTy
  where
    retTypeP :: TokenParser Type
    retTypeP = typeP <|> (keywordP KwVoid >>= \(_,start,end) -> pure $ Void (Loc start end))


------------------------
-- Fields & Identifiers

fieldP :: TokenParser (WithPos Field)
fieldP = headP <|> tailP <|> fstP <|> sndP
  where
    headP = keywordP KwHead >>= \(_,s,e) -> pure (Head,s,e)
    tailP = keywordP KwTail >>= \(_,s,e) -> pure (Tail,s,e)
    fstP = keywordP KwFst >>= \(_,s,e) -> pure (Fst,s,e)
    sndP = keywordP KwSnd >>= \(_,s,e) -> pure (Snd,s,e)

-------------------------
-- Expressions

funCallEP :: TokenParser Expr
funCallEP = do
  ((funId,args),start,end) <- funCallP
  pure $ FunCallE (Loc start end) funId args

parenOrTupleP :: TokenParser Expr
parenOrTupleP = do
  (_,start,_) <- symbolP SymParenLeft
  e <- exprP
  closeExpr e <|> closeTuple e start
  where
    closeExpr :: Expr -> TokenParser Expr
    closeExpr e = do
      _  <- symbolP SymParenRight
      pure e
    closeTuple :: Expr -> SourcePos -> TokenParser Expr
    closeTuple e1 start = do
      _ <- symbolP SymComma
      e2 <- exprP
      (_,_,end) <- symbolP SymParenRight
      pure $ Tuple (Loc start end) e1 e2

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
        try funCallEP <|> identP
  where
    emptyListP = symbolP SymBracketLR >>= \(_,start,end) ->
      pure $ EmptyList (Loc start end)
    identP = idP >>= \(ident,s,e) -> pure $ Ident (Loc s e) ident

fieldLookupP :: TokenParser Expr
fieldLookupP = atomP >>= opFieldLookupP
  where
    opFieldLookupP :: Expr -> TokenParser Expr
    opFieldLookupP e = try (do
      (_,start,_) <- symbolP SymDot
      (field,_,end) <- fieldP
      opFieldLookupP (ExprLookup (Loc start end) (ExprField e field))) <|> pure e

unOpP :: TokenParser Expr
unOpP = bangExprP <|> negExprP <|> fieldLookupP
  where
    bangExprP = symbolP SymBang >>= \(_,s,e) -> UnOp (Loc s e) Not <$> unOpP
    negExprP = symbolP SymMinus >>= \(_,s,e) -> UnOp (Loc s e) Neg <$> unOpP

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
      opValP (BinOp (Loc (getStart val) (getEnd val')) op val val')) <|> pure val

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
      opTermP (BinOp (Loc (getStart term) (getEnd term)) op term term')) <|> pure term

listP :: TokenParser Expr
listP = try (formP >>= opFormP) <|> formP
  where
    opP :: TokenParser BinaryOp
    opP = symbolP SymColon >> pure Cons
    -- Cons operator `:` associates to the right
    opFormP :: Expr -> TokenParser Expr
    opFormP form = try (do
      op <- opP
      list <- listP -- Recurse first to get end pos
      pure $ BinOp (Loc (getStart form) (getEnd list)) op form list) <|> pure form

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
      opListP (BinOp (Loc (getStart list) (getEnd list')) op list list')) <|> pure list

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

ifP :: TokenParser Stmt
ifP = do
  (_,startPos,_) <- keywordP KwIf
  (cond,_,_) <- parensP exprP
  (thenStmts,_,end) <- bracesP (many stmtP)
  mElseStmts <- optional $ do
    _ <- keywordP KwElse
    bracesP $ many stmtP
  let (elseStmts,endPos) = case mElseStmts of
        Nothing -> ([],end)
        Just (stmts,_,end') -> (stmts,end')
  pure $ If (Loc startPos endPos) cond thenStmts elseStmts


whileP :: TokenParser Stmt
whileP = do
  (_,start,_) <- keywordP KwWhile
  (cond,_,_) <- parensP exprP
  (stmts,_,end) <- bracesP $ many stmtP
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
  -- This permits mixed order of funDecls and varDecls. The reordering may cause
  -- odd behaviour and should be caught somewhere with an error
  (funDecls, varDecls) <- partitionEithers <$> many ((Left <$> (lookAhead isFunDeclLookAhead >> funOrMutualDeclP)) <|> (Right <$> varDeclP))
  Program varDecls funDecls <$ eof
  where
    isFunDeclLookAhead :: TokenParser ()
    isFunDeclLookAhead = void $
      (satisfy (isKeyword KwMutual) >> satisfy (isSymbol SymBraceLeft)) <|>
      (satisfy isIdent >> satisfy (isSymbol SymParenLeft))

    isKeyword :: Keyword -> Positioned Parser.Tokens.Token -> Bool
    isKeyword kw (Positioned _ _ _ _ (Keyword kw')) = kw == kw'
    isKeyword _ _ = False

    isSymbol :: Symbol -> Positioned Parser.Tokens.Token -> Bool
    isSymbol sym (Positioned _ _ _ _ (Symbol sym')) = sym == sym'
    isSymbol _ _ = False

    isIdent :: Positioned Parser.Tokens.Token -> Bool
    isIdent (Positioned _ _ _ _ (IdToken _)) = True
    isIdent _ = False


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

-- FIXME: Refactor these to make them less clunky
symbolP :: Symbol -> TokenParser (WithPos ())
symbolP s = token test S.empty
  where
    test t | tokenVal t == Symbol s = Just ((), startPos t, endPos t)
    test _ = Nothing

keywordP :: Keyword -> TokenParser (WithPos ())
keywordP k = token test S.empty
  where
    test t | tokenVal t == Keyword k = Just ((), startPos t, endPos t)
    test _ = Nothing

idP :: TokenParser (WithPos T.Text)
idP = token test S.empty
  where
    test (Positioned start end _ _ (IdToken i)) = Just (i,start,end)
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

-- | Parses expression of form `[e]`, where e is parsed by the parser provided
--   in the argument.
bracketsP :: TokenParser a -> TokenParser (WithPos a)
bracketsP = betweenP (symbolP SymBracketLeft) (symbolP SymBracketRight)

-- | Parses expression of form `{e}`, where e is parsed by the parser provided
--   in the argument.
bracesP :: TokenParser a -> TokenParser (WithPos a)
bracesP = betweenP (symbolP SymBraceLeft) (symbolP SymBraceRight)
