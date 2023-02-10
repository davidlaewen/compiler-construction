{-# LANGUAGE FlexibleInstances, DerivingStrategies, TypeFamilies #-}
module Parser.Definition (
  Lexer,
  TokenParser,
  TokenStream(..)
) where

import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec
import Parser.Tokens qualified as PT

type Lexer = Parsec Void Text


type TokenParser = Parsec Void TokenStream


newtype TokenStream = TokenStream [PT.Token]
  deriving Show

instance Stream TokenStream where
  type Token TokenStream = PT.Token
  type Tokens TokenStream = [PT.Token]

  take1_ (TokenStream []) = Nothing
  take1_ (TokenStream (t:ts)) = Just (t, TokenStream ts)


instance VisualStream TokenStream where

instance TraversableStream TokenStream where
