{-# LANGUAGE DeriveFunctor, InstanceSigs, FlexibleInstances, DerivingStrategies, TypeFamilies #-}
module Parser.Definition where

import Control.Applicative
import Control.Monad
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec
import Parser.Tokens qualified as PT
import Syntax.Program
import Data.Set qualified as S

type Lexer = Parsec Void Text


type TokenParser = Parsec Void [PT.Token]


newtype TokenStream = TokenStream [PT.Token]
  deriving Show

instance Stream TokenStream where
  type Token TokenStream = PT.Token
  type Tokens TokenStream = [PT.Token]
