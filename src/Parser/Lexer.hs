{-# LANGUAGE OverloadedStrings, ImportQualifiedPost, GADTs #-}

module Parser.Lexer (lexer) where


import Parser.Definition
import Parser.Tokens
import Data.Text (Text)
import Data.Text qualified as T
import Data.Char (isAlphaNum, isSpace, isPunctuation)
import Text.Megaparsec hiding ( Token )
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad (void)
import Data.Void (Void)
import qualified Parser.Tokens as PT




----------------------------
-- Comments

lineCommentP :: Lexer ()
lineCommentP = L.skipLineComment "//"

blockCommentP :: Lexer ()
blockCommentP = L.skipBlockCommentNested "/*" "*/"

-- Space consumer
sc :: Lexer ()
sc = L.space space1 lineCommentP blockCommentP

-- Non-empty space
-- scne :: Lexer ()
-- scne = space1 >> sc


--------------------------
-- Basic operators defined in terms of the space consumer

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

symbol :: Text -> Lexer Text
symbol = L.symbol sc


--------------------------------
-- Built-in data types

intL :: Lexer Token
intL = IntLit <$> decimal <* withRecovery recovery (notFollowedBy letterChar)
  where
    recovery e = do
      -- FIXME: Possibly provide better error message here
      -- _ <- fail . T.unpack $ "Misformed integer"
      registerParseError e
      void $ some alphaNumChar

boolL :: Lexer Token
boolL = BoolLit <$> (trueP <|> falseP) <* notFollowedBy alphaNumChar
  where
    trueP = symbol "True" >> pure True
    falseP = symbol "False" >> pure False

sCharL :: Lexer Char
sCharL = satisfy isSChar <?> "string character"
  where
    isSChar c = isAlphaNum c || isSpace c || (isPunctuation c && c /= '"' && c /= '\'')

charL :: Lexer Token
charL = do
  _ <- char '\''
  ch <- sCharL
  _ <- char '\''
  notFollowedBy alphaNumChar
  pure $ CharLit ch


----------------------
-- Identifiers

-- | Parses an identifer without trailing whitespace
identL :: Lexer Token
identL = do
  name <- T.cons <$> letterChar <*> (T.pack <$> many (alphaNumChar <|> char '_'))
  pure $ IdToken name


keywordL :: Lexer Token
keywordL = choice $ keyword <$> keywords
  where
    keyword kw = do
      _ <- string (T.pack $ show kw) <* notFollowedBy alphaNumChar
      pure $ Keyword kw


symbolL :: Lexer Token
symbolL = choice $ (\sym -> string (T.pack (show sym)) >> pure (Symbol sym)) <$> symbols


lex1 :: Lexer Token
lex1 = try intL <|> symbolL <|> try boolL <|> charL <|> try keywordL <|> withRecovery recover (hidden identL)
  where
    recover e = do
      registerParseError e
      _ <- lexeme anySingle
      lex1

withPosition :: Lexer a -> Lexer (Positioned a)
withPosition l = do
  startOffset <- getOffset
  beginPos <- getSourcePos
  x <- l
  endOffset <- getOffset
  endPos <- getSourcePos
  return $ Positioned beginPos endPos startOffset (endOffset - startOffset) x

lexProgram :: Lexer [Positioned PT.Token]
lexProgram = do
  sc
  many (lexeme $ withPosition lex1) <* eof


lexer :: FilePath -> T.Text -> Either (ParseErrorBundle T.Text Void) TokenStream
lexer filePath input = snd $ runParser' (TokenStream input <$> lexProgram) (initialState filePath input)
