{-# LANGUAGE GADTs #-}

module Parser.Tokens (
  Token(..),
  Keyword(..),
  keywords,
  isKeyword,
  Symbol(..),
  symbols
) where

import Data.Text ( Text )
import qualified Data.Text as T


data Token = IntLit Int | BoolLit Bool | CharLit Char
           | IdToken T.Text
           | Symbol Symbol
           | Keyword Keyword
  deriving (Eq, Ord)

instance Show Token where
  show (IntLit n) = show n
  show (BoolLit b) = if b then "True" else "False"
  show (CharLit c) = ['\'', c, '\'']
  show (IdToken t) = T.unpack t
  show (Symbol s) = show s
  show (Keyword k) = show k


----------------------
-- Keywords

data Keyword where
  KwVar     :: Keyword

  KwIf      :: Keyword
  KwElse    :: Keyword
  KwWhile   :: Keyword
  KwReturn  :: Keyword

  KwMutual  :: Keyword

  KwInt     :: Keyword
  KwBool    :: Keyword
  KwChar    :: Keyword
  KwVoid    :: Keyword

  KwHead    :: Keyword
  KwTail    :: Keyword
  KwFst     :: Keyword
  KwSnd     :: Keyword

  deriving (Eq, Ord, Enum, Bounded)


instance Show Keyword where
  show KwVar     = "var"
  show KwIf      = "if"
  show KwElse    = "else"
  show KwWhile   = "while"
  show KwReturn  = "return"

  show KwMutual  = "mutual"

  show KwInt     = "Int"
  show KwBool    = "Bool"
  show KwChar    = "Char"
  show KwVoid    = "Void"

  show KwHead    = "hd"
  show KwTail    = "tl"
  show KwFst     = "fst"
  show KwSnd     = "snd"

-- | List of all keywords
keywords :: [Keyword]
keywords = enumFromTo minBound maxBound

isKeyword :: Text -> Bool
isKeyword s = s `elem` (T.pack . show <$> keywords)




-----------------------
-- Symbols

-- | IMPORTANT: For two symbols with overlapping prefixes, the longer symbol
-- must be enumerated before the shorter one
data Symbol where
  SymColonColon    :: Symbol
  SymRightArrow    :: Symbol
  SymBracketLR     :: Symbol

  -- Operators
  SymPipePipe      :: Symbol
  SymAndAnd        :: Symbol

  SymEqEq          :: Symbol
  SymBangEq        :: Symbol
  SymLessThanEq    :: Symbol
  SymGreaterThanEq :: Symbol
  SymLessThan      :: Symbol
  SymGreaterThan   :: Symbol

  SymBang          :: Symbol
  SymPlus          :: Symbol
  SymMinus         :: Symbol
  SymAst           :: Symbol
  SymSlash         :: Symbol
  SymPercent       :: Symbol
  SymColon         :: Symbol

  -- Syntax
  SymEq            :: Symbol
  SymComma         :: Symbol
  SymDot           :: Symbol
  SymSemicolon     :: Symbol

  -- Parens
  SymParenLeft     :: Symbol
  SymParenRight    :: Symbol
  SymBracketLeft   :: Symbol
  SymBracketRight  :: Symbol
  SymBraceLeft     :: Symbol
  SymBraceRight    :: Symbol


  -- etc.
  deriving (Eq, Ord, Enum, Bounded)


instance Show Symbol where
  show SymColonColon    = "::"
  show SymRightArrow    = "->"
  show SymBracketLR     = "[]"

  -- Operators
  show SymPipePipe      = "||"
  show SymAndAnd        = "&&"

  show SymEqEq          = "=="
  show SymBangEq        = "!="
  show SymLessThanEq    = "<="
  show SymGreaterThanEq = ">="
  show SymLessThan      = "<"
  show SymGreaterThan   = ">"

  show SymBang          = "!"
  show SymPlus          = "+"
  show SymMinus         = "-"
  show SymAst           = "*"
  show SymSlash         = "/"
  show SymPercent       = "%"
  show SymColon         = ":"

  -- Syntax
  show SymEq            = "="
  show SymComma         = ","
  show SymDot           = "."
  show SymSemicolon     = ";"

  -- Parens
  show SymParenLeft     = "("
  show SymParenRight    = ")"
  show SymBracketLeft   = "["
  show SymBracketRight  = "]"
  show SymBraceLeft     = "{"
  show SymBraceRight    = "}"

-- | List of all symbols
symbols :: [Symbol]
symbols = enumFromTo minBound maxBound
