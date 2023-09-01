module TypeInference.Types (
  UVar, TVar, UType(..), UScheme(..),
) where

import qualified Data.Text as T
import qualified Data.Set as S

type UVar = Int
type TVar = T.Text

data UType = Int | Bool | Char | Void
           | Prod UType UType | List UType
           | Fun [UType] UType
           | UVar UVar
           | TVar TVar
  deriving (Show, Eq)

data UScheme = UScheme (S.Set UVar) UType
  deriving (Show)
