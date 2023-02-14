{-# LANGUAGE FlexibleInstances, DerivingStrategies, TypeFamilies #-}
module Parser.Definition (
  Lexer,
  Positioned(..),
  TokenParser,
  TokenStream(..)
) where

import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec
import qualified Parser.Tokens as PT

type Lexer = Parsec Void Text

type TokenParser = Parsec Void TokenStream

newtype TokenStream = TokenStream [Positioned PT.Token]
  deriving (Show)

data Positioned a = Positioned
  { startPos :: SourcePos,
    endPos :: SourcePos,
    tokenLength :: Int,
    tokenVal :: a }
  deriving (Eq, Ord, Show)

instance Stream TokenStream where
  type Token TokenStream = Positioned PT.Token
  type Tokens TokenStream = [Positioned PT.Token]

  take1_ (TokenStream []) = Nothing
  take1_ (TokenStream (t:ts)) = Just (t, TokenStream ts)


instance VisualStream TokenStream where

instance TraversableStream TokenStream where
