module Parser.Tokens where

import Syntax.Common ( Id )
import Data.Text ( Text )
import Data.Text qualified as T


data Token = IntLit Integer | BoolLit Bool | CharLit Char
           | IdToken Id
           | Symbol Symbol
           | Keyword Keyword
  deriving (Eq, Ord, Show)


----------------------
-- Keywords

data Keyword where
  KwVar     :: Keyword

  KwIf      :: Keyword
  KwElse    :: Keyword
  KwWhile   :: Keyword
  KwReturn  :: Keyword

  KwInt     :: Keyword
  KwBool    :: Keyword
  KwChar    :: Keyword
  KwVoid    :: Keyword

  KwEmpty   :: Keyword
  KwPrint   :: Keyword
  KwIsEmpty :: Keyword

  KwHead    :: Keyword
  KwTail    :: Keyword
  KwFst     :: Keyword
  KwSnd     :: Keyword

  deriving (Eq, Ord, Enum, Bounded)


instance Show Keyword where
  show KwVar    = "var"
  show KwIf     = "if"
  show KwElse   = "else"
  show KwWhile  = "while"
  show KwReturn = "return"

  show KwInt  = "Int"
  show KwBool = "Bool"
  show KwChar = "Char"
  show KwVoid = "Void"

  show KwEmpty   = "[]"
  show KwPrint   = "print"
  show KwIsEmpty = "isEmpty"

  show KwHead = "hd"
  show KwTail = "tl"
  show KwFst  = "fst"
  show KwSnd  = "snd"

-- | List of all keywords
keywords :: [Keyword]
keywords = enumFromTo minBound maxBound

isKeyword :: Text -> Bool
isKeyword s = s `elem` (T.pack . show <$> keywords)




-----------------------
-- Symbols

data Symbol where
  SymEq          :: Symbol
  SymComma       :: Symbol
  SymSemicolon   :: Symbol
  SymColonColon :: Symbol
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
  deriving (Eq, Ord, Enum, Bounded)


instance Show Symbol where
  show SymEq          = "="
  show SymComma       = ","
  show SymSemicolon   = ";"
  show SymColonColon = "::"
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

-- | List of all symbols
symbols :: [Symbol]
symbols = enumFromTo minBound maxBound
