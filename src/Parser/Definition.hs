{-# LANGUAGE DeriveFunctor, InstanceSigs, FlexibleInstances, DerivingStrategies #-}
module Parser.Definition where

import Control.Applicative
import Control.Monad
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec

type Parser = Parsec Void Text
