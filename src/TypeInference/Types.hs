module TypeInference.Types (
  UVar, TVar, UType(..), UScheme(..),
) where

import qualified Data.Text as T
import qualified Data.Set as S
import Parser.Tokens (Keyword(..))

type UVar = Int
type TVar = T.Text

data UType = Int | Bool | Char | Void
           | Prod UType UType | List UType
           | Fun [UType] UType
           | Data T.Text
           | UVar UVar
           | TVar TVar
  deriving Eq

-- TODO: Write a pretty printer
instance Show UType where
  show Int = show KwInt
  show Bool = show KwBool
  show Char = show KwChar
  show Void = show KwVoid
  show (Prod ty1 ty2) = "(" <> show ty1 <> "," <> show ty2 <> ")"
  show (List ty) = "[" <> show ty <> "]"
  show (Fun argTys retTy) = unwords (show <$> argTys) <>
    (if null argTys then "-> " else " -> ") <> show retTy
  show (Data t) = T.unpack t
  show (UVar i) = "u" <> show i
  show (TVar t) = T.unpack t

data UScheme = UScheme (S.Set UVar) UType

instance Show UScheme where
  show (UScheme vars ty) = "∀ " <> unwords (showUVar <$> S.toList vars) <> ". " <> show ty
    where
      showUVar i = "u" <> show i
