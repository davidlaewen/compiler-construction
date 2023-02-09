{-# LANGUAGE OverloadedStrings, ImportQualifiedPost, GADTs #-}

module Parser.Lexer (
  sc,
  scne,
  intP,
  charP,
  boolP,
  identP,
  Keyword(..),
  keywordP,
  Symbol(..),
  symbolP,
  parensP,
  bracketsP,
  bracesP
) where


import Parser.Definition
import Data.Text (Text)
import Data.Text qualified as T
import Data.Char (isAlphaNum, isSpace, isPunctuation)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char.Lexer (decimal, signed)




----------------------------
-- Comments

lineCommentP :: Parser ()
lineCommentP = L.skipLineComment "//"

blockCommentP :: Parser ()
blockCommentP = L.skipBlockCommentNested "/*" "*/"

-- Space consumer
sc :: Parser ()
sc = L.space space1 lineCommentP blockCommentP

-- Non-empty space
scne :: Parser ()
scne = space1 >> sc


--------------------------
-- Basic operators defined in terms of the space consumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc


--------------------------------
-- Built-in data types

intP :: Parser Integer
intP = signed space decimal

boolP :: Parser Bool
boolP = trueP <|> falseP
  where
    trueP = symbol "True" >> pure True
    falseP = symbol "False" >> pure False

sCharP :: Parser Char
sCharP = satisfy isSChar <?> "string character"
  where
    isSChar c = isAlphaNum c || isSpace c || (isPunctuation c && c /= '"' && c /= '\'')

charP :: Parser Char
charP = do
  symbolP SymSingleQuote
  ch <- sCharP
  symbolP SymSingleQuote
  pure ch


----------------------
-- Identifiers


nameReserved :: Text -> Parser ()
nameReserved s | isKeyword s = fail . T.unpack $ "Keyword " <> s <> " cannot be used as an identifier."
               | otherwise = return ()

-- | Parses an identifer without trailing whitespace
identP :: Parser Text
identP = do
  name <- T.cons <$> alphaNumChar <*> (T.pack <$> many (alphaNumChar <|> char '_'))
  nameReserved name
  pure name


----------------------
-- Keywords

data Keyword where
  KwVar    :: Keyword

  KwIf     :: Keyword
  KwElse   :: Keyword
  KwWhile  :: Keyword
  KwReturn :: Keyword

  KwInt    :: Keyword
  KwBool   :: Keyword
  KwChar   :: Keyword
  KwVoid   :: Keyword

  KwEmpty  :: Keyword

  deriving (Eq, Enum, Bounded)


instance Show Keyword where
  show KwVar = "var"
  show KwIf  = "if"
  show KwElse = "else"
  show KwWhile = "while"
  show KwReturn = "return"

  show KwInt = "Int"
  show KwBool = "Bool"
  show KwChar = "Char"
  show KwVoid = "Void"

  show KwEmpty = "[]"

-- | List of all keywords
keywords :: [Keyword]
keywords = enumFromTo minBound maxBound


-- | Parses a given keyword
keywordP :: Keyword -> Parser ()
keywordP kw = do
  _ <- string (T.pack $ show kw) <* notFollowedBy alphaNumChar
  pure ()


isKeyword :: Text -> Bool
isKeyword s = s `elem` (T.pack . show <$> keywords)


-----------------------
-- Symbols

data Symbol where
  SymEq          :: Symbol
  SymComma       :: Symbol
  SymSemicolon   :: Symbol
  SymColonColon :: Symbol
  SymSingleQuote :: Symbol
  SymDoubleQuote :: Symbol
  SymRightArrow  :: Symbol

  -- Operators
  SymNot   :: Symbol
  SymNeg   :: Symbol
  SymPlus  :: Symbol
  SymMinus :: Symbol

  -- Parens
  SymParenLeft    :: Symbol
  SymParenRight   :: Symbol
  SymBracketLeft  :: Symbol
  SymBracketRight :: Symbol
  SymBraceLeft    :: Symbol
  SymBraceRight   :: Symbol


  -- etc.
  deriving (Eq, Enum, Bounded)


instance Show Symbol where
  show SymEq          = "="
  show SymComma       = ","
  show SymSemicolon   = ";"
  show SymColonColon = "::"
  show SymSingleQuote = "'"
  show SymDoubleQuote = "\""
  show SymRightArrow  = "->"

  show SymNot   = "!"
  show SymNeg   = "-"
  show SymPlus  = "+"
  show SymMinus = "-"

  show SymParenLeft    = "("
  show SymParenRight   = ")"
  show SymBracketLeft  = "["
  show SymBracketRight = "]"
  show SymBraceLeft    = "{"
  show SymBraceRight   = "}"


-- | Does not parse trailing whitespace
symbolP :: Symbol -> Parser ()
symbolP sym = do
  _ <- string (T.pack (show sym))
  pure ()


-------------------
-- Parens

-- | Parses expression of form (e), where e is parsed by the parser provided
--   in the argument.
--   The provided parser must parse its own whitespace
parensP :: Parser a -> Parser a
parensP parser = do
  symbolP SymParenLeft
  sc
  res <- parser
  symbolP SymParenRight
  pure res

-- | Parses expression of form [e], where e is parsed by the parser provided
--   in the argument.
--   The provided parser must parse its own whitespace
bracketsP :: Parser a -> Parser a
bracketsP parser = do
  symbolP SymBracketLeft
  sc
  res <- parser
  symbolP SymBracketRight
  pure res

-- | Parses expression of form {e}, where e is parsed by the parser provided
--   in the argument.
--   The provided parser must parse its own whitespace
bracesP :: Parser a -> Parser a
bracesP parser = do
  symbolP SymBraceLeft
  sc
  res <- parser
  symbolP SymBraceRight
  pure res
